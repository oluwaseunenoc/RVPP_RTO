$title Real time Redispatch Operation of RES-based Virtual Power Plants for FCR and aFRR provision

$onEmpty

$onText
    Developed by

    Oluwaseun Enoch Oladimeji and Lukas Sigrist
    Institute of Research in Technology
    Higher School of Engineering - ICAI
    UNIVERSIDAD PONTIFICIA COMILLAS
    Alberto Aguilera 23
    28015 Madrid, Spain

    oenoch@comillas.edu
    23rd February, 2022
$offText

$onText
    This program ascertains the possibility of evacuation of
    inertia and primary frequency control services offered
    by a portfolio connected to the distribution grid.
    
    ****************************************************************
    
    The program takes all data from the excel file Redispatch.xlsx.
    The input data are from the sheets named:
        Buses
        Generator
        Loads
        Branches_Lines
        Branches_Transformers
        
    While the outputs are written into the sheets:
        DC-PF
        AC-PF-NF
        AC-PF-LTVM
        
    The choice of where to write the output is based on the 
    network model selected to obtain the solution.
    
    ***************************************************************
    
    To set the network model, use this key (for now the
    selection is made within GAMS. In the future, the selection
    will be allowed from within excel itself):
    
    pGlobal['SolverSelect'] = 1 for DC-PF
    pGlobal['SolverSelect'] = 2 for AC-PF-NF (Relaxed Network flow model)
    pGlobal['SolverSelect'] = 3 for AC-PF-LTVM (Approximated Logarithmic xform
    Voltage magnitude/Paco's model).
    
    ***************************************************************
    
    To avoid errors, it is expected that the topmost row in all
    sheets are not modified. They contain the headings that are used
    directly in the GAMs model contained in this file.
    
    Additionally, the first columns of these sheets, i.e.,
        Buses!A
        Generator!A
        Loads!A 
    are used as indexes for the respective data that they contain.
    As such, the user should ensure that these columns have unique
    identifiers for the model to read all the data correctly.
    
    ***************************************************************
    
    Finally, the user should ensure that after modification(s) is/are
    made in the excel, the excel workbook should be closed before
    running GAMs. If the workbook is not closed, the results will
    not be written from GAMs.
    
    ***************************************************************
$offText




$onMulti

$GDXIN Redisp.gdx

$onUNDF



******************
***   Scalar   ***
******************
$onFold
Scalars
    sPeriod            'Number of periods used in energy markets [hr]'  
    sSubperiod         'Number of subdivisions for real time optimization'
    sPstart            'Starting period for optimization [-]' 
    sSubstart          'Starting subperiod for optimization [-]'
    sP2
    
    sTsoCommand        'TSO command for setpoint change'

    sI                 'Scalar to control iteration for LTVM [-]'
    
    ;

$offFold


******************
***    Sets    ***
******************
$onFold 
Sets
   b            'Index of buses'
   x            'Index of bus types (transmission or distribution)'
   
   n            'Index of piecewise linear models of line flows, S_{ij}'
                /n1*n100/

   v            'Index of RVPP units'
   y            'Index of RVPP unit type (generator, demand, ess etc)'

   h            'Index of periods'
   m            'Index of subperiods' 
   ;
   
Alias (b,bb);
Alias (v,vv);

* Subsets
Sets
    bx                      [b,x   ]    'Index of buses connected to Xmission\distribution network'
    vb                      [v,b   ]    'Index of RVPP units at buses'
    vy                      [v,y   ]    'Index of RVPP types'
    ;
    
$offFold

****************************
***      Parameters      ***
****************************
$onFold
Parameters
    pGlobal
    
    pBusData               [b,    *]    'Bus parameters'
    pTradeData             [b,h,  *]
    
    pVppData               [v,    *]    'RVPP parameters'
    pDispatchData          [v,h,  *]    'RVPP periodic dispatches'
    
    pPrevDel               [v,h,  m]    'RVPP dispatch at time t-1'   
    pRealTimeData          [v,h,  m]    'RVPP real-time machine delivery'
    pExpectedDel           [v,h,  m]    'Expected delivery after application of TSO command'
    
    pBranchData            [b,bb, *]    'Line parameters'
    
    pRegCost               [v,h,  m]    'Regulation cost of RRVPP units'
    
    ;


$load sPeriod, sSubPeriod, sPstart, sSubStart, sTsoCommand


$eval sPeriod sPeriod
$eval sSubperiod sSubPeriod
$eval sPstart sPstart
$eval sP2   sPstart+1
$eval sSubStart sSubStart


set h /1 * %sPeriod% / ;
set m /1 * %sSubPeriod%/ ;


$load b, x, bx, v, y, vb, vy
$load pGlobal, pBusData, pTradeData, pVppData,
$load pDispatchData, pBranchData,
$load pPrevDel, pRealTimeData, pExpectedDel



Scalar
    sDelta             'Power-energy conversion factor [-]';
    sDelta = card[m] ;

set ef[h,m]            'Effective set of periods and subperiods';
    

** Set the IPFC time to a single time index
** Set the afrr effective time based on start period and subperiod    
*if (pGlobal['SolverSelect' ] <= 2,
*         ef['%sPstart%','1' ]                       = YES ;
*    ;
*else
*         ef['%sPstart%',m   ] $ (ORD[m]>=sSubStart) = YES ;
*         ef['%sP2%',    m   ] $ (ORD[m]< sSubStart) = YES ;
*    );
    
**** EDIT HERE

ef['%sPstart%',m   ] $ (ORD[m]>=sSubStart) = YES ;
ef['%sP2%',    m   ] $ (ORD[m]< sSubStart) = YES ;

    
$offFold

* Uncomment the below function to view if set and parameter data are read correctly from Excel
*display b,x,bx;

display pVppData, pDispatchData, sDelta;

$offFold

****************************
* Scheduled Gen Parameters *
****************************
$onFold
Parameters
    pSchEnergy        [v,h]     'Cleared generation in energy market'    
    pSchPfc           [v,h]     'Initial scheduled power for primary frequency control'
    pSchInertia       [v,h]     'Initial scheduled power for inertia provision' 
    pSchPfcTotal                'Total initial scheduled power for primary frequency control'
    pSchInertiaTotal            'Total initial scheduled power for inertia provision' 
    pSchEnergyTotal             'Total initial energy cleared in energy market' 
    
    ;
    
pSchEnergy            [v,h]
        $vy[v,'G']              =  pDispatchData[v,h,'PGen' ] $(pDispatchData[v,h,'Commitment'] <> 0) ;
       
pSchPfc               [v,h]
        $vy[v,'G']              =  pVppData[v,'DeltaFss'    ] *
                                   pVppData[v,'PMax'        ] $(pDispatchData[v,h,'Commitment'] <> 0) ;
pSchInertia           [v,h]
        $vy[v,'G']              =  pVppData[v,'Inertia'     ] *
                                   pVppData[v,'PMaX'        ] $(pDispatchData[v,h,'Commitment'] <> 0) ;

pSchPfcTotal                    =  SUM(v, SUM(h, pSchPfc    [v,h])) ;
       
pSchInertiaTotal                =  SUM(v, SUM(h, pSchInertia[v,h])) ;
             
pSchEnergyTotal                 =  SUM(v, SUM(h, pSchEnergy [v,h])) ;
    


*display pSchEnergy, pSchPfc, pSchInertia, pDispatchData

 
    
    
$offFold


****************************
***    Line Parameters   ***
****************************
$onFold
*** Add branches_lines and branches_Xformer data to same dataframe

* Line reactances
$Ontext
pBranchData[b,bb,'LineX'     ] = pBranchData[bb,b,'LineX'              ]   +
                                        pTrans_data [b,bb,'SpecifiedX'         ]   ;
* Line resistances                                
pBranchData[b,bb,'LineR'     ] = pBranchData[bb,b,'LineR'              ]   +
                                        pTrans_data [b,bb,'SpecifiedR']   ;
* Line ratings                                      
pBranchData[b,bb,'Rate1'] = pBranchData[bb,b,'Rate1'         ]   +
                                        pTrans_data [b,bb,'Rate1'              ]   ;

$OffText

pBranchData[b,bb,'LineX'     ] $(pBranchData[b,bb,'LineX'     ]=0)   =
                                 pBranchData[bb,b,'LineX'     ]      ;
                                       
pBranchData[b,bb,'LineR'     ] $(pBranchData[b,bb,'LineR'     ]=0)   =
                                 pBranchData[bb,b,'LineR'     ]      ;
                                       
pBranchData[b,bb,'ChargingB' ] $(pBranchData[b,bb,'ChargingB' ]=0)   =
                                 pBranchData[bb,b,'ChargingB' ]      ;
                                        
pBranchData[b,bb,'InService' ] $(pBranchData[b,bb,'InService' ]=0)   =
                                 pBranchData[bb,b,'InService' ]      ;
                                       
pBranchData[b,bb,'Rate1'     ] $(pBranchData[b,bb,'Rate1'     ]=0)   =
                                 pBranchData[bb,b,'Rate1'     ]      ;

* Compute the admittance parameters for all lines and add to pBranchData dataframe

pBranchData[b,bb,'g'    ]$pBranchData[b,bb,'Rate1' ] = -pBranchData[b,bb,'LineR'  ] /
                                                   (sqr(pBranchData[b,bb,'LineR'  ]) + sqr(pBranchData[b,bb,'LineX']));
                                                   
pBranchData[bb,b,'g'    ]$(pBranchData[b,bb,'g'    ]=0)         =  pBranchData[b,bb,'g'  ];

pBranchData[b,bb,'bbb'  ]$pBranchData[b,bb,'Rate1' ] =  pBranchData[b,bb,'LineX'  ] /
                                                   (sqr(pBranchData[b,bb,'LineR'  ]) + sqr(pBranchData[b,bb,'LineX']));
                                                   
pBranchData[bb,b,'bbb'  ]$(pBranchData[b,bb,'bbb'  ]=0)         =  pBranchData[b,bb,'bbb'];


pBranchData(b,bb,'th')$(pBranchData(b,bb,'Rate1') and pBranchData(b,bb,'LineX') and pBranchData(b,bb,'LineR'))   =
                                                            arctan(pBranchData(b,bb,'LineX')/(pBranchData(b,bb,'LineR')));
pBranchData(b,bb,'th')$(pBranchData(b,bb,'Rate1') and pBranchData(b,bb,'LineX') and pBranchData(b,bb,'LineR')=0) = pi/2;
pBranchData(b,bb,'th')$(pBranchData(b,bb,'Rate1') and pBranchData(b,bb,'LineR') and pBranchData(b,bb,'LineX')=0) = 0;
pBranchData(bb,b,'th')$ pBranchData(b,bb,'Rate1') = pBranchData(b,bb,'th');

pBranchData(b,bb,'LineZ')$ pBranchData(b,bb,'Rate1')     = sqrt(sqr(pBranchData(b,bb,'LineX')) + sqr(pBranchData(b,bb,'LineR')));
pBranchData(bb,b,'LineZ')$(pBranchData(b,bb,'LineZ')=0)  = pBranchData(b,bb,'LineZ');

*
Parameter pCx               [b,bb    ]  'Matrix of available lines';

pCx[b,bb]$(pBranchData[b,bb,'Rate1'] and pBranchData[bb,b,'Rate1']) = 1;
pCx[b,bb]$(pCx[bb,b]) = 1;



Parameter pConductance      [b,bb    ]  'Lines conductance matrix';

pConductance[b,bb]$(pBranchData[b,bb,'g']        and     pBranchData[bb,b,'Rate1']) = 
                                                         pBranchData[bb,b,'g'] ;
pConductance[b,bb]$(pConductance[bb,b   ])  =            pBranchData[bb,b,'g'] ;
pConductance[b,b ]                          =   -SUM(bb, pBranchData[bb,b,'g']);


Parameter pSusceptance      [b,bb    ]  'Lines susceptance matrix';

pSusceptance[b,bb]$(pBranchData[b,bb,'bbb']      and     pBranchData[bb,b,'Rate1']) =
                                                         pBranchData[bb,b,'bbb'  ]   ;
                                                         
pSusceptance[b,bb]$(pSusceptance[bb,b      ]) =          pBranchData[bb,b,'bbb'  ]   ;
*pSusceptance[b,b ]                            = -SUM(bb, pBranchData[bb,b,'bbb'] - 0.5*pBranchData[b,bb,'Charging B (pu)']);
pSusceptance[b,b ]                            = -SUM(bb, pBranchData[bb,b,'bbb'  ])  ;



pBranchData(b,bb,'InService')$ (pBranchData(b,bb,'InService')=0) = pBranchData(bb,b,'InService');
pBranchData(b,bb,'LineX')    $ (pBranchData(b,bb,'LineX')    =0) = pBranchData(bb,b,'LineX');
pBranchData(b,bb,'LineR')    $ (pBranchData(b,bb,'LineR')    =0) = pBranchData(bb,b,'LineR');
pBranchData(b,bb,'ChargingB')$ (pBranchData(b,bb,'ChargingB')=0) = pBranchData(bb,b,'ChargingB');
pBranchData(b,bb,'Rate1')    $ (pBranchData(b,bb,'Rate1')    =0) = pBranchData(bb,b,'Rate1');

pBranchData(b,bb,'bij')      $  pBranchData(b,bb,'Rate1')        = 1/pBranchData(b,bb,'LineX');

pBranchData(b,bb,'z')        $  pBranchData(b,bb,'Rate1')        = sqrt(sqr(pBranchData(b,bb,'LineX')) +
                                                                        sqr(pBranchData(b,bb,'LineR')));
pBranchData(bb,b,'z')        $ (pBranchData(b,bb,'z')        =0) = pBranchData(b,bb,'z');

pBranchData(b,bb,'th')$(pBranchData(b,bb,'Rate1') and pBranchData(b,bb,'LineX') and pBranchData(b,bb,'LineR'))   = arctan(pBranchData(b,bb,'LineX')/(pBranchData(b,bb,'LineR')));
pBranchData(b,bb,'th')$(pBranchData(b,bb,'Rate1') and pBranchData(b,bb,'LineX') and pBranchData(b,bb,'LineR')=0) = pi/2;
pBranchData(b,bb,'th')$(pBranchData(b,bb,'Rate1') and pBranchData(b,bb,'LineR') and pBranchData(b,bb,'LineX')=0) = 0;
pBranchData(bb,b,'th')$ pBranchData(b,bb,'Rate1') = pBranchData(b,bb,'th');



Parameter pCx               [b,bb    ]  'Matrix of available lines';

pCx[b,bb]$(pBranchData[b,bb,'Rate1'] and pBranchData[bb,b,'Rate1']) = 1;
pCx[b,bb]$(pCx[bb,b]) = 1;




Parameter pL                [n       ]  'Number of piecewise linear divisions for line limit computation' ;
Loop(n,
    pL[n] = ord(n);
    ) ;

* Uncomment below to view the matrices of the available lines and admittance parameters.
display pBranchData, pCx ;

Parameter
    pActivePloss            [b,bb,h,m]  'Active power loss initialized to zero and to be updated after first run'
    pReactivePloss          [b,bb,h,m]  'Reactive power loss initialized to zero and to be updated after first run'
    ;
    
$offFold

****************************
***    Regulation cost  ***
****************************
$onFold
Parameters
    pRhs    [v,h,m]
    pLhs    [v,h,m]
    pRegCost[v,h,m]     'Regulation cost update(s) based on speed of regulation units'
    
    ;
    
pLhs    [v,h,'%sSubstart%']
       $ef[h,'%sSubstart%'] = abs(pRealTimeData  [v,h,'%sSubstart%']  - 
                                  pPrevDel       [v,h,'%sSubstart%']) ;

pRhs    [v,h,'%sSubstart%']
       $ef[h,'%sSubstart%'] =     pGlobal        ['firstOrderLevel']  *
                              abs(pExpectedDel   [v,h,'%sSubstart%']  -
                                  pPrevDel       [v,h,'%sSubstart%']) ;
    
pRegCost[v,h,'%sSubstart%']   
       $ef[h,'%sSubstart%'] =( pGlobal       ['multiplyLevel'  ] *  pVppData   [v,'RegUpCost'    ]) $
                             ( pLhs          [v,h,'%sSubstart%'] <  pRhs       [v,h,'%sSubstart%']) +
                               pVppData      [v,'RegUpCost'    ]                                    $
                             ( pLhs          [v,h,'%sSubstart%'] >= pRhs       [v,h,'%sSubstart%']) ;
                 
$offFold



****************************
***      Variables       ***
****************************
$onFold
Variables
** RTM variables
    vCostRedispatch                      'Total cost of redispatch [€]'
    
    vActivePflow            [b,bb,h,m]   'Active power flow through line l in time period t [MW]'
    vReactivePflow          [b,bb,h,m]   'Reactive power flow through line l in time period t [MVar]'
    vActivePloss            [b,bb,h,m]   'Active power loss through line l in time period t [MW]'
    vReactivePloss          [b,bb,h,m]   'Reactive power loss through line l in time period t [MVar]'
    
    vTsoCommand             [b,   h,m]   'Power to be delivered to PCCs based on TSO demand [MW]'
    
    vDelRtmP                [v,   h,m]   'Final active power RVPP delivery at real time [MW]'
    vDelRtmQ                [v,   h,m]   'Final reactive power RVPP delivery at real time [MVar]'
    
    vGridQ                  [b,   h,m]   'Reactive power obtained from/sent to grid [MVar]'
    
    vVoltageMag             [b,   h,m]   'Voltage magnitude at bus b in time period t [V]'
    vVoltageMagRelaxed      [b,   h,m]   'Relaxed voltage magnitude [-]'
    
    vVoltageAngle           [b,   h,m]   'Voltage angle at bus b in time period t [rad]'
    
** IPFC Variables
    vCostIpfc                            'Forecast Cost for ipfc service provision [€]'
    
    vPowerPfcMktP           [v,   h  ]   'Actual active power dispatch of unit u wrt primary freq control mkt'
    vPowerInertiaMktP       [v,   h  ]   'Actual active power dispatch of unit u wrt inertia mkt'
    
    vXEnergyMkt             [v,   h  ]   'Slack variable to relax power dispatch wrt energy mkt'
    vXPfcMkt                [v,   h  ]   'Slack variable to relax power dispatch wrt primary freq control mkt'
    vXInertiaMkt            [v,   h  ]   'Slack variable to relax power dispatch wrt inertia mkt'
    
    vGridP                  [b,   h  ]   'Active power obtained from/sent to grid'
    ;
    
Positive Variables
    vRegDown                [v,   h,m]   'Active power up regulation of RVPP unit in time period t [MW]'     
    vRegUp                  [v,   h,m]   'Active power down regulation of RVPP unit in time period t [MW]'
    
    vGridPDown              [b,   h,m]   'Active power sent to grid [MW]'
    vGridPUp                [b,   h,m]   'Active power obtained from grid [MW]'
    
    vEssEnergy              [v,   h,m]   'Energy stored at the end of time period t [MWh]'
    vEssChUp                [v,   h,m]   'Charging power up regulation of ESSs in time period t [MW]'
    vEssChDown              [v,   h,m]   'Charging power down regulation of ESSs in time period t [MW]'
    vEssDisUp               [v,   h,m]   'Discharging power up regulation of ESSs in time period t [MW]'
    vEssDisDown             [v,   h,m]   'Discharging power down regulation of ESSs in time period t [MW]'
    ;
    
Binary variables
    bSwitchUpDown           [v,   h,m]   '[-]'
    bCommit                 [v,   h,m]   'On/off status of ess [-]'
    ;
    
$offFold




****************************
***   Variable bounds    ***
****************************
$onFold
*************************************************************************
* Line flows
vActivePflow.up    [b,bb,h,m]          =  pBranchData[b,bb,'Rate1'] * pBranchData[b,bb,'InService'] ;                                       
vActivePflow.lo    [b,bb,h,m]          = -pBranchData[b,bb,'Rate1'] * pBranchData[b,bb,'InService'] ;

vReactivePflow.up  [b,bb,h,m]          =  pBranchData[b,bb,'Rate1'] * pBranchData[b,bb,'InService'] ;
vReactivePflow.lo  [b,bb,h,m]          = -pBranchData[b,bb,'Rate1'] * pBranchData[b,bb,'InService'] ;


*Active and Reactive power dispatch per units
vDelRtmP.up        [v,h,m]   $ef[h,m]  = pVppData[v,'PMax'] * pDispatchData[v,h,'Commitment'] / sDelta ;       
vDelRtmP.lo        [v,h,m]   $ef[h,m]  = pVppData[v,'PMin'] * pDispatchData[v,h,'Commitment'] / sDelta ;
        
vDelRtmQ.up        [v,h,m]   $ef[h,m]  =  pVppData[v,'QMax'] * pDispatchData[v,h,'Commitment'] / sDelta ;        
vDelRtmQ.lo        [v,h,m]   $ef[h,m]  =  pVppData[v,'QMin'] * pDispatchData[v,h,'Commitment'] / sDelta ;
        

* voltage angles
vVoltageAngle.up   [b,h,m]             =  Pi/2 ;
vVoltageAngle.lo   [b,h,m]             = -Pi/2 ;

vVoltageAngle.fx   [b,h,m]   $bx[b,'T']= 0   ;

vVoltageMag.up     [b,h,m]             = pBusData[b, 'NormalVmax'] ;
vVoltageMag.lo     [b,h,m]             = pBusData[b, 'NormalVmin'] ;

*Bounds on Up and Down regulation of RVPP units
vRegDown.up        [v,h,m]   $ef[h,m]  =  ( pDispatchData[v,h,'RegMaxDown'   ]          +
                                            pDispatchData[v,h,'RegMaxDownV2' ]          $
                                           (pDispatchData[v,h,'RegMaxDown'   ] = 0))    *
                                            pDispatchData[v,h,'Commitment'   ] / sDelta ;
        
vRegUp.up          [v,h,m]   $ef[h,m]  =  ( pDispatchData[v,h,'RegMaxUp'     ]          +
                                            pDispatchData[v,h,'RegMaxUpV2'   ]          $
                                           (pDispatchData[v,h,'RegMaxUp'     ] = 0))    *
                                            pDispatchData[v,h,'Commitment'   ] / sDelta ;

*Energy Storage variables
vEssEnergy.lo      [v,h,m]    $
    (vy[v,'E'] AND (ORD[h] = CARD[h])) =    pGlobal   ['EssLbFinal'          ]          *
                                            pVppData  [v,'PMax'              ]          *
                                            pDispatchData[v,h,'Commitment'   ] / sDelta ;
vEssEnergy.lo      [v,h,m ]   $
    (vy[v,'E'] AND ef[h,m ])           =    pVppData  [v,'PMin'              ]          *
                                            pDispatchData[v,h,'Commitment'   ] / sDelta ;
vEssEnergy.up      [v,h,m ]   $
    (vy[v,'E'] AND ef[h,m ])           =    pVppData  [v,'PMax'              ]          *
                                            pDispatchData[v,h,'Commitment'   ] / sDelta ;  

display vDelRtmP.lo, vDelRtmP.up ;


$offFold

**********************************
**** IPFC variable bounds     ****
**********************************
$onFold
*Active and Reactive power dispatch per units

*if (pGlobal['SolverSelect'] <= 2,
*    vDelRtmP.lo          [v, h,m]$(vy[v,'G'])   =  pVppData      [v,'PMin'        ] * pDispatchData[v,h,'Commitment'] ;
*    vDelRtmP.up          [v, h,m]$(vy[v,'G'])   =  pDispatchData [v,h,'PAvailable'] * pDispatchData[v,h,'Commitment'] ;
*    
*    vPowerInertiaMktP.lo [v, h  ]$(vy[v,'G'])   =  pSchInertia   [v,h             ] ;
*    vPowerPfcMktP.lo     [v, h  ]$(vy[v,'G'])   =  pSchPfc       [v,h             ] ;
*    
*    vDelRtmQ.up          [v, h,m]$(vy[v,'G'])   =  pVppData      [v,'QMax'        ] ;
*    vDelRtmQ.lo          [v, h,m]$(vy[v,'G'])   =  pVppData      [v,'QMin'        ] ;
*    
*    vDelRtmP.fx          [v, h,m]$(vy[v,'D'])   =  pDispatchData [v,h,'PGen'      ] * pDispatchData [v,h,'Commitment'] ;
*    vDelRtmQ.fx          [v, h,m]$(vy[v,'D'])   =  pDispatchData [v,h,'QLoad'     ] * pDispatchData [v,h,'Commitment'] ;
*
*   );


$offFold

**********************************
***   Equations Declaration   ****
**********************************
$onFold
Equations
*** Common equations

eNodal_bal_active_DC           'DC-PF Active power balance at buses'
eLine_power                    'DC-PF power flow through lines'

eNodal_bal_active_LTVM         'Linear AC-PF Active power balance at PCC(s)'
eNodal_bal_reactive_LTVM       'Linear AC-PF Reactive power balance at PCC(s)'
eLine_power_AC_P_LTVM          'Linear AC-PF active power flow through lines'
eLine_power_AC_Q_LTVM          'Linear AC-PF reactive power flow through lines'
eActive_loss_LTVM              'Linear AC-PF active power loss'
eReactive_loss_LTVM            'Linear AC-PF reactive power loss'
eApparent_power                'Linear AC-PF apparent power'

eLine_power_ACFULL_P           'FULL AC-PF active power flow through lines'
eLine_power_ACFULL_Q           'FULL AC-PF reactive power flow through lines'
eNodal_bal_active_ACFULL       'FULL AC-PF active power nodal balance'
eNodal_bal_reactive_ACFULL     'FULL AC-PF reactive power nodal balance'


*** RTM equations

eCost_Redispatch               'Cost of real-time redispatch'   
eDelivery_VPP                  'Final  delivery of RVPP units'

eTSO_command                   'Balance of TSO command based on PCCs'

eDem_energy_min                'Minimum Demand energy over operation horizon'

eVPP_ramp_down                 'Ramp down production limitation of RVPP unit'
eVPP_ramp_up                   'Ramp up production limitation of RVPP unit'

eEss_balance                   'Power balance equation in the ESS unit'
eEss_injection_up              'Power up injection of ESS unit'
eEss_injection_down            'Power down injection of ESS unit'
eEss_ch_max                    'Maximum power that can be transferred to ESS unit'
eEss_dis_max                   'Maximum power that can be produced by ESS unit'

eDynamic                       'Check if unit response is at expected level'

    
*** IPFC equations

eCost_ipfc                     'Cost of i+pfc service provision for AC_PF'

eObj_rex_energy1               'Relaxation 1 wrt to energy market'
eObj_rex_energy2               'Relaxation 2 wrt to energy market'

eObj_rex_PFC1                  'Relaxation 1 wrt to PFC market'
eObj_rex_PFC2                  'Relaxation 2 wrt to PFC market'

eObj_rex_inertia1              'Relaxation 1 wrt to inertia market'
eObj_rex_inertia2              'Relaxation 2 wrt to inertia market'

eEnergy_total                  'Total energy traded in the energy market'
ePFC_total                     'Total power traded in the PFC market'
eInertia_total                 'Total power traded in the inertia market'

eE_PFC_I_Total                 'Total energy, Inertia and PFC less than gen capacity'
eE_PFC_I_Total_LB              'Lower bound of total energy, Inertia and PFC'

eVoltage_mag_ref

   ;

$offFold

**********************************
******* Equations Description ****
**********************************

**********************************
***    Objective Function RTM  ***
**********************************
$onFold
eCost_Redispatch..                         vCostRedispatch     * sDelta                            =E=
                              SUM(v,
                              SUM(ef[h,m],
                                    ((     vDelRtmP     [v,h,m]*(pVppData  [v,'DamCost'    ] ))    +
                                     (     vRegUp       [v,h,m]*(pRegCost  [v,h,m          ]       +
                                           pGlobal                         [  'Lambda'     ] ))    +
                                     (     vRegDown     [v,h,m]*(pRegCost  [v,h,m          ]       +
                                           pGlobal                         [  'Lambda'     ] )) )  +
                              SUM(b$bx[b,'T'],
                                     (     vGridPUp     [b,h,m]* pGlobal   [  'PenaltyCost']       +
                                           vGridPDown   [b,h,m]* pGlobal   [  'PenaltyCost'] )) )) ;


**********************************
*** Other common constraints   ***
**********************************
eDelivery_VPP[v,h,m] $ef[h,m]..            vDelRtmP     [v,h,m            ]                    =E=
                                      (    pDispatchData[v,h,'PGen'       ]/ sDelta )          + 
                                     ((    vRegUp       [v,h,m            ]                    -
                                           vRegDown     [v,h,m            ]) $(NOT vy[v,'D'])) +
                                     ((    vRegDown     [v,h,m            ]                    -
                                           vRegUp       [v,h,m            ]) $(    vy[v,'D'])) ;

            
* When there are more buses connected to the Xmission system, u need to compute
* the equivalent reactances to determine the power flow divisions from the
* short cct power values.

$offFold

**********************************
***  Objective Function IPFC   ***
**********************************
$onFold
* Use minimization of the Q-flow on the line
eCost_ipfc..                                           vCostIpfc                                     =E=
                              SUM(v$(vy[v,'G']),
                              SUM(ef[h,m],             pGlobal['EmCcost'] * vXEnergyMkt   [v,h]      +
                                                       pGlobal['PmCost' ] * vXPfcMkt      [v,h]      +
                                                       pGlobal['ImCost' ] * vXInertiaMkt  [v,h] ) )  +
                              SUM(b,
                              SUM(ef[h,m], 
                              SUM(bb$pCx[bb,b],   0.01*pGlobal['EmCcost'] * vReactivePflow [b,bb,h,m]))) ;


***  Offer relaxations  ***
eObj_rex_energy1    [v,h,m]
    $(ef[h,m] AND vy[v,'G'])..                        -vXEnergyMkt        [v,h  ]  =L=
                                                       vDelRtmP           [v,h,m]  -
                                                       pSchEnergy         [v,h  ]  ;

eObj_rex_energy2    [v,h,m]
    $(ef[h,m] AND vy[v,'G'])..                         vDelRtmP           [v,h,m]  -
                                                       pSchEnergy         [v,h  ]  =L=
                                                       vXEnergyMkt        [v,h  ]  ;

eObj_rex_PFC1       [v,h,m]
    $(ef[h,m] AND vy[v,'G'])..                        -vXPfcMkt           [v,h  ]  =L=
                                                       vPowerPfcMktP      [v,h  ]  -
                                                       pSchPfc            [v,h  ]  ;

eObj_rex_PFC2       [v,h,m]
    $(ef[h,m] AND vy[v,'G'])..                         vPowerPfcMktP      [v,h  ]  -
                                                       pSchPfc            [v,h  ]  =L=
                                                       vXPfcMkt           [v,h  ]  ;

eObj_rex_inertia1   [v,h,m]
    $(ef[h,m] AND vy[v,'G'])..                        -vXInertiaMkt       [v,h  ]  =L=
                                                       vPowerInertiaMktP  [v,h  ]  -
                                                       pSchInertia        [v,h  ]  ;

eObj_rex_inertia2   [v,h,m]
    $(ef[h,m] AND vy[v,'G'])..                         vPowerInertiaMktP  [v,h  ]  -
                                                       pSchInertia        [v,h  ]  =L=
                                                       vXInertiaMkt       [v,h  ]  ;

*****************************

*** Total quantity of I+PFC services
eEnergy_total               ..                         pSchEnergyTotal             =E=
                               SUM(v$(vy[v,'G']),
                               SUM(ef[h,m],            vDelRtmP         [v,h,m]))  ;
                               
ePFC_total                  ..                         pSchPfcTotal                =E=
                               SUM(v$(vy[v,'G']),
                               SUM(ef[h,m],            vPowerPfcMktP    [v,h  ]))  ;

eInertia_total              ..                         pSchInertiaTotal            =E=
                               SUM(v$(vy[v,'G']),
                               SUM(ef[h,m],            vPowerInertiaMktP[v,h  ]))  ;


eE_PFC_I_Total_LB   [v,h,m]
    $(ef[h,m] AND vy[v,'G'])..                         pVppData          [v,'PMin']  =L=
                                                       vDelRtmP          [v,h,m   ]  +
                                                       vPowerPfcMktP     [v,h     ]  +
                                                       vPowerInertiaMktP [v,h     ]  ;

eE_PFC_I_Total      [v,h,m ]
    $(ef[h,m] AND vy[v,'G'])..                         vDelRtmP          [v,h,m   ]  +
                                                       vPowerPfcMktP     [v,h     ]  +
                                                       vPowerInertiaMktP [v,h     ]  =L=
                                                       pVppData          [v,'PMax']  ;
                                                 
eVoltage_mag_ref     [b,h,m]
    $(ef[h,m] AND bx [b,'T'])..                        vVoltageMagRelaxed[b,h,m   ]  =E=
                                                       0                             ;

$offFold

**********************************
***      DC PF Formulation     ***
**********************************
$onFold                   
eNodal_bal_active_DC   [b,h,m ]
                    $ef[h,m].. SUM(v$vb  [v, b],  vDelRtmP      [v,h,m                  ] $(                             NOT vy[v,'D']))  +
                               SUM(v$vb  [v, b],  vPowerPfcMktP [v,h                    ] $((pGlobal['SolverSelect']<3) AND vy[v,'G']))   +
                                  (               vGridPUp      [b,h,m                  ]                                                 -
                                                  vGridPDown    [b,h,m                  ]                                                 -
                                         (        pTradeData    [b,h,'TradedPower'      ] / sDelta)                                       -
                                                  vTsoCommand   [b,h,m                  ])$(bx[b,'T'] AND (pGlobal['SolverSelect']>2))    +
                                         (     (  vGridP        [b,h                    ]                                                 - 
                                                  pSchPfcTotal                           )$(bx[b,'T'] AND (pGlobal['SolverSelect']<3)))   =E=
                               SUM(v$vb  [v, b],  vDelRtmP      [v,h,m                  ] $(                                  vy[v,'D'])) +
                               SUM(bb$pCx[bb,b],  vActivePflow  [b,bb,h,m               ]                                             )   ;
                               

eLine_power          [b,bb,h,m]
    $(ef[h,m] AND pCx[b,bb])..                    vActivePflow  [b,bb,h,m               ]     /
                                                  pGlobal       ['PowerBase'            ]     =E=
                                              (1 /pBranchData   [b,bb,'LineX'           ])    *
                                              (   vVoltageAngle [b,h,m                  ]     -
                                                  vVoltageAngle [bb,h,m                 ])    ;
$offFold


**********************************
***   LTVM AC PF Formulation   ***
**********************************
$onFold
eNodal_bal_active_LTVM[b,h,m]
                   $ef[h,m]..  SUM(v$vb  [v, b],  vDelRtmP      [v,h,m            ] $(                          NOT   vy[v,'D']))  +
                                  (               vGridPUp      [b,h,m            ]                                                -
                                                  vGridPDown    [b,h,m            ]                                                -
                                         (        pTradeData    [b,h,'TradedPower'] / sDelta)                                      -
                                                  vTsoCommand   [b,h,m            ])$(bx[b,'T'] AND (pGlobal['SolverSelect']>2))   +
                               SUM(bb$pCx[bb,b],  vActivePloss  [b,bb,h,m         ]          )                                     +
                                         (     (  vGridP        [b,h              ]                                                - 
                                                  pSchPfcTotal                     )$(bx[b,'T'] AND (pGlobal['SolverSelect']<3)))  =E=
                               SUM(v$vb  [v, b],  vDelRtmP      [v,h,m            ] $(                                vy[v,'D']))  +
                               SUM(bb$pCx[bb,b],  vActivePflow  [b,bb,h,m         ]                                             )  ;
                               
                                           
eNodal_bal_reactive_LTVM[b,h,m]
                     $ef[h,m]..SUM(v$vb  [v, b],  vDelRtmQ       [v,h,m           ] $(NOT vy[v,'D']))  +
                                               (  vGridQ         [b,h,m           ] $(    bx[b,'T']))  -
                               SUM(bb$pCx[bb,b],  vReactivePloss [b,bb,h,m        ]                 )  =E=
                               SUM(v$vb  [v, b],  vDelRtmQ       [v,h,m           ] $(    vy[v,'D']))  +
                               SUM(bb$pCx[bb,b],  vReactivePflow [b,bb,h,m        ]                 )  ;


eLine_power_AC_P_LTVM[b,bb,h,m]
   $(ef[h,m] AND pCx[b,bb])..                    vActivePflow       [b,bb,h,m    ]   /
                                                 pGlobal            ['PowerBase' ]   =E=
                                                 pConductance       [b,bb        ]   *
                                        (        vVoltageMagRelaxed [b,h,m       ]   -
                                                 vVoltageMagRelaxed [bb,h,m      ])  -
                                                 pSusceptance       [b,bb        ]   *
                                        (        vVoltageAngle      [b,h,m       ]   -
                                                 vVoltageAngle      [bb,h,m      ])  ;
                               
eLine_power_AC_Q_LTVM[b,bb,h,m]
    $(ef[h,m] AND pCx[b,bb])..                  vReactivePflow      [b,bb,h,m        ]   /
                                                pGlobal             ['PowerBase'     ]   =E=
                                              - pSusceptance        [b,bb            ]   *
                                         (      vVoltageMagRelaxed  [b,h,m           ]   -
                                                vVoltageMagRelaxed  [bb,h,m          ])  -
                                                0.5*pBranchData     [b,bb,'ChargingB']   *
                                         (      1                                        +
                                                2*vVoltageMagRelaxed[b,h,m           ])  -
                                                pConductance        [b,bb            ]   *
                                         (      vVoltageAngle       [b,h,m           ]   -
                                                vVoltageAngle       [bb,h,m          ])  ;
                               

eActive_loss_LTVM    [b,bb,h,m]
    $(ef[h,m] AND pCx[b,bb])..                  vActivePloss        [b,bb,h,m    ]   /
                                                pGlobal             ['PowerBase' ]   =E=
                                                pActivePloss        [b,bb,h,m    ]   ;

                             
eReactive_loss_LTVM  [b,bb,h,m] 
    $(ef[h,m] AND pCx[b,bb])..                  vReactivePloss      [b,bb,h,m    ]   /
                                                pGlobal             ['PowerBase' ]   =E=
                                                pReactivePloss      [b,bb,h,m    ]   ;


eApparent_power       [n,b,bb,h,m]
    $(ef[h,m] AND pCx[b,bb])..((SIN(          2*Pi  * pL            [n           ]     /
                                                CARD (pL                           ))  -
                                SIN(          2*Pi  *(pL            [n           ]-1)  /
                                                CARD (pL                           ))) *
                                                      vActivePflow  [b,bb,h,m    ]   ) -
                              ((COS(          2*Pi  * pL            [n           ]     /
                                                CARD (pL                           ))  -
                                COS(          2*Pi  *(pL            [n           ]-1)  /
                                                CARD (pL                           ))) *
                                                      vReactivePflow[b,bb,h,m    ]   ) =L=
                                                      pBranchData   [bb,b,'Rate1']     *
                                SIN(          2*Pi                                     /
                                                CARD (pL                           ))  ;

 
eTSO_command        [h,m]
                 $ef[h,m]..     SUM(b$bx[b,'T'],      vTsoCommand   [b,h,m       ] )   -
                                                      sTsoCommand / sDelta =E=
                                                      0                                ;


$offFold

**********************************
***   FULL AC PF Formulation   ***
**********************************
$onFold

eLine_power_ACFULL_P[b,bb,h,m]
   $(ef[h,m] AND pCx[b,bb])..   vActivePflow       [b,bb,h,m    ]   /
                                pGlobal            ['PowerBase' ]   =E=
                               (vVoltageMag        [b,   h,m    ]   *
                                vVoltageMag        [b,   h,m    ]   *
                                cos(pBranchData    [bb,b,'th'   ])  -
                                vVoltageMag        [b,   h,m    ]   *
                                vVoltageMag        [bb,  h,m    ]   *
                                cos(vVoltageAngle  [b,h,m       ]   -
                                vVoltageAngle      [bb,h,m      ]   +
                                pBranchData        [bb,b,'th'   ])) /
                                pBranchData        [bb,b,'z'    ]   ;


eLine_power_ACFULL_Q[b,bb,h,m]
   $(ef[h,m] AND pCx[b,bb])..   vReactivePflow     [b,bb,h,m    ]  /
                                pGlobal            ['PowerBase' ]   =E=
                               (vVoltageMag        [b,   h,m    ]  *
                                vVoltageMag        [b,   h,m    ]  *
                                sin(pBranchData    [bb,b,'th'   ] )-
                                vVoltageMag        [b,   h,m    ]  *
                                vVoltageMag        [bb,  h,m    ]  *
                                sin(
                                vVoltageAngle      [b,h,m       ]   -
                                vVoltageAngle      [bb,h,m      ]   +
                                pBranchData        [bb,b,'th'   ])) /
                                pBranchData        [bb,b,'z'    ]   -
                                pBranchData        [bb,b,'b'    ]   *
                                vVoltageMag        [b,   h,m    ]   *
                                vVoltageMag        [b,   h,m    ]/2 ;
                                

eNodal_bal_active_ACFULL[b,h,m]
                   $ef[h,m]..  SUM(v$vb  [v, b],  vDelRtmP      [v,h,m            ] $(NOT   vy[v,'D']))  +
                                  (               vGridPUp      [b,h,m            ]                      -
                                                  vGridPDown    [b,h,m            ]                      -
                                         (        pTradeData    [b,h,'TradedPower'] / sDelta)            -
                                                  vTsoCommand   [b,h,m            ])$ bx[b,'T']          =E=
                               SUM(v$vb  [v, b],  vDelRtmP      [v,h,m            ] $(      vy[v,'D']))  +
                               SUM(bb$pCx[bb,b],  vActivePflow  [b,bb,h,m         ]                   )  ;

                                           
eNodal_bal_reactive_ACFULL[b,h,m]
                     $ef[h,m]..SUM(v$vb  [v, b],  vDelRtmQ       [v,h,m           ] $(NOT vy[v,'D']))  +
                                               (  vGridQ         [b,h,m           ] $(    bx[b,'T']))  =E=
                               SUM(v$vb  [v, b],  vDelRtmQ       [v,h,m           ] $(    vy[v,'D']))  +
                               SUM(bb$pCx[bb,b],  vReactivePflow [b,bb,h,m        ]                 )  ;

$offFold


**********************************
***     Ramp constraints       ***
**********************************
$onFold
eDem_energy_min[v    ]
        $vy    [v,'D']..                               pVppData      [v,'EtMin'       ]           =L=
                                        SUM(ef[h,m],   pDispatchData [v,h,'PGen'      ] /sDelta   +
                                                       vDelRtmP      [v,h,m           ] )         ;

eVPP_ramp_down [v,h,m]
        $ef    [h,m  ]..                   (           pVppData      [v,'Gen0'        ] $ (ORD[h] = sPstart)           +
                                                       pDispatchData [v,h,'PGen'      ] $ (ORD[h] > sPstart) )/sDelta  -
                                                       vDelRtmP      [v,h,m           ]                                =L=
                                           (           pVppData      [v,'RampDown'    ]                                *
                                           (           pVppData      [v,'Gen0Commit'  ] $ (ORD[h] = sPstart)           +
                                                       pDispatchData [v,h,'Commitment'] $ (ORD[h] > sPstart) )         +
                                           (           pVppData      [v,'RampShutdown']                                *
                                           (1 -        pDispatchData [v,h,'StartUp'   ] )                  ) )/sDelta  ;


eVPP_ramp_up  [v,h,m]
       $ef    [h,m  ]..                                vDelRtmP      [v,h,m           ]                                - 
                                          (            pVppData      [v,'Gen0'        ] $ (ORD[h] = sPstart)           +
                                          (            pDispatchData [v,h,'PGen'      ] $ (ORD[h] > sPstart) ))/sDelta =L=
                                          (            pVppData      [v,'RampUp'      ]                                *
                                           (           pVppData      [v,'Gen0Commit'  ] $ (ORD[h] = sPstart)           +
                                                       pDispatchData [v,h,'Commitment'] $ (ORD[h] > sPstart) )         +
                                           (           pVppData      [v,'RampStartup' ]                                *
                                                       pDispatchData [v,h,'StartUp'   ]                    ) )/sDelta  ;

$offFold


**********************************
***   Energy Storage units     ***
**********************************
$onFold
eEss_balance [v,h,m]$
    (vy[v,'E'] AND ef[h,m  ] AND
    pDispatchData  [v,h,'PGen' ]<> 0)..               vEssEnergy    [v,h,m           ]                         =E=
                                          (           pVppData      [v,'Gen0'        ]  $ (ORD[h] = sPstart)   +
                                                      vEssEnergy    [v,h-1,m         ]  $ (ORD[h] > sPstart)   *
                                          (       1-( pGlobal       ['Gamma'         ]  /2400) )    )/sDelta   +
                                          ( (         pDispatchData [v,h,'PGen'      ]                         +
                                                      vEssChUp      [v,h,m           ]                         -
                                                      vEssChDown    [v,h,m           ])                        *
                                                      pVppData      [v,'EssChEff'    ]  $
                                          (           pDispatchData [v,h,'PGen'      ]< 0) ) /sDelta           -
                                          ( (         pDispatchData [v,h,'PGen'      ]                         +
                                                      vEssDisUp     [v,h,m           ]                         -
                                                      vEssDisDown   [v,h,m           ])                        /
                                            (         pVppData      [v,'EssDisEff'   ]) $
                                          (           pDispatchData [v,h,'PGen'      ]> 0) ) /sDelta           ;

eEss_injection_up[v,h,m]$
    (vy[v,'E'] AND ef[h,m  ])..                       vRegUp        [v,h,m           ]      =E=
                                                      vEssDisUp     [v,h,m           ]      -
                                                      vEssChUp      [v,h,m           ]      ;

eEss_injection_down[v,h,m]$
    (vy[v,'E'] AND ef[h,m  ])..                       vRegDown      [v,h,m           ]      =E=
                                                      vEssDisDown   [v,h,m           ]      -
                                                      vEssChDown    [v,h,m           ]      ;

eEss_ch_max[v,h,m]$
    (vy[v,'E'] AND ef[h,m  ])..                       pVppData      [v,'PGen'        ]  $
                                          (           pDispatchData [v,h,'PGen'      ]< 0)  +
                                                      vEssChUp      [v,h,m           ]      -
                                                      vEssChDown    [v,h,m           ]      =L=
                                                      pVppData      [v,'EssChCap']   *
                                                      bCommit       [v,h,m           ]      ;


eEss_dis_max[v,h,m]$
    (vy[v,'E'] AND ef[h,m  ])..                       pVppData      [v,'PGen'        ]  $
                                          (           pDispatchData [v,h,'PGen'      ]> 0)  +
                                                      vEssDisUp     [v,h,m           ]      -
                                                      vEssDisDown   [v,h,m           ]      =L=
                                                      pVppData      [v,'EssDisCap'   ]      *
                                          (       1 - bCommit       [v,h,m           ])     ;

$offFold




****************************************
***  Redispatch Models definition    ***
****************************************
$onFold
Model mCommon_RTM   /
                eCost_Redispatch
                eDelivery_VPP
                eTSO_command
                eDem_energy_min
                eVPP_ramp_down
                eVPP_ramp_up
*                eEss_balance
*                eEss_injection_up
*                eEss_injection_down
*                eEss_ch_max
*                eEss_dis_max
                /;

Model mCommon_IPFC   /
                eCost_ipfc
                eObj_rex_energy1
                eObj_rex_energy2
                eObj_rex_PFC1
                eObj_rex_PFC2
                eObj_rex_inertia1
                eObj_rex_inertia2
                eEnergy_total
                ePFC_total
                eInertia_total
                eE_PFC_I_Total
                eE_PFC_I_Total_LB
                eVoltage_mag_ref
                /;
                

Model mRedispatch_DC  /
                mCommon_RTM
                eLine_power
                eNodal_bal_active_DC
                /;

Model mRedispatch_AC /
                mCommon_RTM
                eLine_power_AC_P_LTVM                
                eLine_power_AC_Q_LTVM
                eActive_loss_LTVM
                eReactive_loss_LTVM     
                eNodal_bal_active_LTVM
                eNodal_bal_reactive_LTVM           
                eApparent_power
                /;
                
Model mRedispatch_ACFULL /
                mCommon_RTM
                eLine_power_ACFULL_P                
                eLine_power_ACFULL_Q 
                eNodal_bal_active_ACFULL
                eNodal_bal_reactive_ACFULL              
                eApparent_power
                /;

Model mIPFC_DC  /
                mRedispatch_DC
                +mCommon_IPFC
                -mCommon_RTM
                /;

Model mIPFC_AC /
                mRedispatch_AC
                +mCommon_IPFC
                -mCommon_RTM
                /;
                

  
Option OPTCR   = 0 ;
Option OPTCA   = 0 ;
Option iterlim = 1e8 ;
Option limcol  = 10 ;
option limrow  = 200 ;
option mip     = cplex ;
option reslim  = 72000000 ;




file opt cplex option file /cplex.opt/;
put  opt;
put 'lpmethod 4'/;
put 'threads 4'/;
put 'solvefinal 0'/;
put 'nodefileind 2'/;
put 'workmem 15700'/;
put 'iis yes/'
putclose;


$offFold

**********************************
***  Parameters for results    ***
**********************************
$onFold
Parameters
    pState                                'Convergence status of optimization problem'
    pCost                                 'Forecast of Cost incurred in the current market session'
    
    pVoltageMagnitude       [b,h,m     ]  'Voltage magnitude'
    pVoltage_angle          [b,h,m     ]  'Voltage angle'
    pCurrent_squared        [b,bb,h,m  ]  'Square of line current used for estimating active and reactive line losses'
        
    pRedispatch_final_P     [h,m       ]  'Final active power source/sink to the PCC'
    pRedispatch_final_Q     [h,m       ]  'Final reactive power source/sink to the PCC'
    
    pIPFC_final_P                         'Final active power source/sink to the PCC'
    pIPFC_final_Q                         'Final reactive power source/sink to the PCC'
    
    pDispatches             [v,h,m,*   ]  'Dispatches'
    pBranchFlows            [b,bb,h,m,*]  'Power flows through lines'
    
    pActivePower            [v,h,m     ]  'Active power generation/demand by VPP units'
    pReg                    [v,h,m     ]
    
    ;
    
$offFold


*********************************
***Solve the I+PFC/RTM problem***
*********************************
$onText
    pGlobal['SolverSelect'] = 1 uses DC power flow model to solve IPFC
    pGlobal['SolverSelect'] = 2 uses AC power flow model to solve IPFC
    pGlobal['SolverSelect'] = 3 uses DC power flow model to solve RTM
    pGlobal['SolverSelect'] = 4 uses AC power flow model to solve RTM
$offText


    if (pGlobal['SolverSelect'] = 1,
    
        
        Solve mRedispatch_ACFULL Using NLP Minimizing vCostRedispatch  ;
        
        pState = mRedispatch_ACFULL.modelstat             ;
        
        pBranchFlows       [b,bb,h,m,'ActivePowerFlow']
                     $(pCx [bb,b                      ]
                     AND ef[h,m                       ])=  vActivePflow.l    [b,bb,h,m]          + EPS ;
                      
        pDispatches        [v,h,m,'ActivePowerGen'    ]
                $ef        [h,m                       ] =  vDelRtmP.l        [v,h,m   ]          + EPS ;
        
        pActivePower       [v,h,m                     ]
                $ef        [h,m                       ] = (vDelRtmP.l        [v,h,m   ]          -
                                                           vRegUp.l          [v,h,m   ]          +
                                                           vRegDown.l        [v,h,m   ] )*sDelta + EPS ;
                
        pReg               [v,h,m                     ]
                     $ef   [h,m                       ] = (-vRegDown.l       [v,h,m   ]          +
                                                            vRegUp.l         [v,h,m   ] )*sDelta + EPS ;
        
        ;

    
         
    elseif pGlobal['SolverSelect'] = 2,
    
        for (sI = 1 to 2,
            
            pActivePloss           [b,bb,h,m] = 0;
            pReactivePloss         [b,bb,h,m] = 0;
            vVoltageMagRelaxed.up  [b,h,m   ] = LOG(pBusData[b, 'NormalVmax']);
            vVoltageMagRelaxed.lo  [b,h,m   ] = LOG(pBusData[b, 'NormalVmin']);                                        
            
            Solve mIPFC_AC Using LP Minimizing vCostIpfc;
            
       
            if (sI > 1,
                pCurrent_squared     [b,bb,h,m]
                            $pCx     [b,bb]     = SQR((vVoltageAngle.l      [b,h,m       ]   -
                                                       vVoltageAngle.l      [bb,h,m      ])) /
                                                  SQR((pBranchData          [b,bb,'LineR']   +
                                                       pBranchData          [b,bb,'LineX'])) +
                                                  SQR((vVoltageMagRelaxed.l [b,h,m       ]   -
                                                       vVoltageMagRelaxed.l [bb,h,m      ])) /
                                                  SQR((pBranchData          [b,bb,'LineR']   +
                                                       pBranchData          [b,bb,'LineX'])) ;
                                               
                pActivePloss        [b,bb,h,m]
                         $pCx        [b,bb]    =  0.5                                        *
                                                       pBranchData          [b,bb,'LineR']   *
                                                       pCurrent_squared     [b,bb,h,m    ]   ;
                                                   
                pActivePloss        [bb,b,h,m]
                         $pCx        [bb,b]    =       pActivePloss         [b,bb,h,m    ]   ;
                
                                                  
                pReactivePloss      [b,bb,h,m]
                           $pCx      [b,bb]    = (0.5                                            *
                                                       pBranchData          [b,bb,'LineX'    ]   *
                                                       pCurrent_squared     [b,bb,h,m        ])  -
                                                    (  pBranchData          [b,bb,'ChargingB']   *
                                                 SQR(  vVoltageMagRelaxed.l [b,h,m           ])) ;
                                               
                pReactivePloss     [bb,b,h,m]
                           $pCx     [bb,b]    = (0.5                                             *
                                                       pBranchData          [bb,b,'LineX'    ]   *
                                                       pCurrent_squared     [bb,b,h,m        ])  -
                                                    (  pBranchData          [bb,b,'ChargingB']   *
                                                 SQR(  vVoltageMagRelaxed.l [bb,h,m          ])) ;

                display pCurrent_squared, pActivePloss, pReactivePloss ;
                

                Solve mIPFC_AC Using LP Minimizing vCostIpfc ;
                
                pState                                            = mIPFC_AC.modelstat                                ;
                
               
*                pIPFC_final_P                                     = SUM(b, SUM(h,  vGridP.l       [b,h     ] ))       ;
                
                pIPFC_final_Q                                     = SUM(b, SUM(ef[h,m], vGridQ.l  [b,h,m   ] )) + EPS ;
                  
                pVoltageMagnitude  [b,h,m                       ] = EXP(vVoltageMagRelaxed.l      [b,h,m   ] )  + EPS ;
                
                pBranchFlows       [b,bb,h,m,'ActivePowerFlow'  ]
                            $pCx   [bb,b                        ] = vActivePflow.l                [b,bb,h,m]    + EPS ;
                            
                pBranchFlows       [b,bb,h,m,'ReactivePowerFlow']
                            $pCx   [bb,b                        ] = vReactivePflow.l              [b,bb,h,m]    + EPS ;
                             
                pBranchFlows       [b,bb,h,m,'LineRatingResult' ]
                            $pCx   [bb,b                        ] = SQRT (
                                                                            SQR(vActivePflow.l    [b,bb,h,m])   +
                                                                            SQR(vReactivePflow.l  [b,bb,h,m]))  + EPS ;
                                                                        
                pBranchFlows       [b,bb,h,m,'LineRatingActual' ]
                            $pCx   [bb,b                        ] = pBranchData[b,bb,'Rate1']     ;
                             
                pBranchFlows       [b,bb,h,m,'%ofLineRating'    ]
                            $pCx   [bb,b                        ] = (pBranchFlows[b,bb,h,m,'LineRatingResult']  /
                                                                     pBranchFlows[b,bb,h,m,'LineRatingActual']) *
                                                                     100                                        + EPS ;
         
                pDispatches         [v,h,m,'ActivePowerGen'     ]
                    $vy[v,'G']                                    = vDelRtmP.l                    [v,h,m   ]    + EPS ;
                    
                pDispatches         [v,h,m,'PFC'                ]
                    $vy[v,'G']                                    = vPowerPfcMktP.l               [v,h     ]    + EPS ;
                    
                pDispatches         [v,h,m,'Inertia'            ]
                    $vy[v,'G']                                    = vPowerInertiaMktP.l           [v,h     ]    + EPS ;
                    
                pDispatches         [v,h,m,'ReactivePowerGen'   ]
                    $vy[v,'G']                                    = vDelRtmQ.l                    [v,h,m   ]    + EPS ;
                 
                
            );
        );
 
        Execute_unload 'FCR.gdx',
            pState                  pVoltageMagnitude 
            pBranchFlows           pDispatches
*            pIPFC_final_P           pIPFC_final_Q

        Execute 'gdxxrw FCR.gdx O=ancillary.xlsx epsOut=0 @writeipfc_ac.txt'
         
        ; 
        



    elseif pGlobal['SolverSelect'] = 3,
    
        vReactivePflow.fx [b,bb,h,m] =  0 ;
        
        Solve mRedispatch_DC Using MIP Minimizing vCostRedispatch  ;
        
        pState = mRedispatch_DC.modelstat ;
       
        pBranchFlows       [b,bb,h,m,'ActivePowerFlow']
                     $(pCx [bb,b                      ]
                     AND ef[h,m                       ])=  vActivePflow.l    [b,bb,h,m]          + EPS ;
                      
        pDispatches        [v,h,m,'ActivePowerGen'    ]
                $ef        [h,m                       ] =  vDelRtmP.l        [v,h,m   ]          + EPS ;
        
        pActivePower       [v,h,m                     ]
                $ef        [h,m                       ] = (vDelRtmP.l        [v,h,m   ]          -
                                                           vRegUp.l          [v,h,m   ]          +
                                                           vRegDown.l        [v,h,m   ] )*sDelta + EPS ;
                
        pReg               [v,h,m                     ]
                     $ef   [h,m                       ] = (-vRegDown.l       [v,h,m   ]          +
                                                            vRegUp.l         [v,h,m   ] )*sDelta + EPS ;
        
        pRedispatch_final_P[h,m                       ]
                    $ef    [h,m                       ] = SUM(b,vGridPUp.l   [b,h,m   ]          -
                                                                vGridPDown.l [b,h,m   ]  )             ;
        



     else
    
        for (sI = 1 to 2,
        
            pActivePloss           [b,bb,h,m] = 0;
            pReactivePloss         [b,bb,h,m] = 0;
            vVoltageMagRelaxed.up  [b,h,m   ] = LOG(pBusData[b, 'NormalVmax']);
            vVoltageMagRelaxed.lo  [b,h,m   ] = LOG(pBusData[b, 'NormalVmin']);                                        
            
            Solve mRedispatch_AC Using MIP Minimizing vCostRedispatch;
            
       
            if (sI > 1,
                pCurrent_squared    [b,bb,h,m]
                             $pCx   [b,bb]    = SQR((vVoltageAngle.l      [b,h,m       ]     -
                                                     vVoltageAngle.l      [bb,h,m      ]))   /
                                                SQR((pBranchData          [b,bb,'LineR']     +
                                                     pBranchData          [b,bb,'LineX']))   +
                                                SQR((vVoltageMagRelaxed.l [b,h,m       ]     -
                                                     vVoltageMagRelaxed.l [bb,h,m      ]))   /
                                                SQR((pBranchData          [b,bb,'LineR']     +
                                                     pBranchData          [b,bb,'LineX']))   ;
                                               
                pActivePloss   [b,bb,h,m]
                        $pCx   [b,bb    ]     =      0.5                                     *
                                                     pBranchData          [b,bb,'LineR']     *
                                                     pCurrent_squared     [b,bb,h,m    ]     ;
                                                   
                pActivePloss   [bb,b,h,m]     =      pActivePloss         [b,bb,h,m    ]     ;
                
                                                  
                pReactivePloss [b,bb,h,m]
                        $pCx   [b,bb    ]     =     (0.5                                      *
                                                     pBranchData          [b,bb,'LineX'    ]  *
                                                     pCurrent_squared     [b,bb,h,m        ]) -
                                                    (pBranchData          [b,bb,'ChargingB']  *
                                                 SQR(vVoltageMagRelaxed.l [b,h,m           ]));
                                               
                pReactivePloss [bb,b,h,m]
                        $pCx   [bb,b    ]     =     (0.5                                      *
                                                     pBranchData          [bb,b,'LineX'    ]  *
                                                     pCurrent_squared     [bb,b,h,m        ]) -
                                                    (pBranchData          [bb,b,'ChargingB']  *
                                                 SQR(vVoltageMagRelaxed.l [bb,h,m          ]));

                display pCurrent_squared, pActivePloss, pReactivePloss ;
                

                Solve mRedispatch_AC Using MIP Minimizing vCostRedispatch;
                
                pState                                              = mRedispatch_AC.modelstat                   ;
                   
                pRedispatch_final_P [h,m                         ]
                            $ef     [h,m                         ]  = SUM(b,  vGridPUp.l       [b,h,m   ]  -
                                                                              vGridPDown.l     [b,h,m   ])       ;
                
                pRedispatch_final_Q [h,m                         ]
                            $ef     [h,m                         ]  = SUM(b,  vGridQ.l         [b,h,m   ])       ;
                
                pVoltageMagnitude   [b,h,m                       ]
                            $ef     [h,m                         ]  = EXP(vVoltageMagRelaxed.l [b,h,m   ]) + EPS ;
                
                pBranchFlows        [b,bb,h,m,'ActivePowerFlow'  ]
                             $(pCx  [bb,b                        ]
                             AND ef [h,m                         ]) = vActivePflow.l           [b,bb,h,m]  + EPS ;
                             
                pBranchFlows        [b,bb,h,m,'ReactivePowerFlow']
                             $(pCx  [bb,b                        ]
                             AND ef [h,m                         ]) = vReactivePflow.l         [b,bb,h,m]  + EPS ;
                             
                pBranchFlows        [b,bb,h,m,'LineRatingResult' ]
                             $(pCx  [bb,b                        ]
                             AND ef [h,m                         ]) = SQRT (  
                                                                      SQR(vActivePflow.l       [b,bb,h,m]) +
                                                                      SQR(vReactivePflow.l     [b,bb,h,m]))+ EPS ;
                                                                        
                pBranchFlows        [b,bb,h,m,'LineRatingActual' ]
                             $(pCx  [bb,b                        ]
                             AND ef [h,m                         ]) = pBranchData          [b,bb,'Rate1'];
                             
                pBranchFlows        [b,bb,h,m,'%ofLineRating'    ]
                             $(pCx  [bb,b                        ]
                             AND ef [h,m                         ]) = (pBranchFlows[b,bb,h,m,'LineRatingResult']  /
                                                                       pBranchFlows[b,bb,h,m,'LineRatingActual']) *
                                                                       100                                        + EPS ;
        
                pDispatches         [v,h,m,'ActivePowerGen'      ]
                            $ef     [h,m                         ]  = vDelRtmP.l               [v,h,m]     + EPS ;
                pDispatches         [v,h,m,'ReactivePowerGen'    ]
                            $ef     [h,m                         ]  = vDelRtmQ.l               [v,h,m]     + EPS ;
                
                pActivePower        [v,h,m                       ]
                        $ef         [h,m                         ]  = (vDelRtmP.l              [v,h,m]     -
                                                                       vRegUp.l                [v,h,m]     +
                                                                       vRegDown.l              [v,h,m])*sDelta + EPS ;
                                                                       
                pReg               [v,h,m                        ]
                             $ef   [h,m                          ]  = (-vRegDown.l             [v,h,m] +
                                                                        vRegUp.l               [v,h,m])*sDelta + EPS ;
        
                
            );
        );
        
    );








