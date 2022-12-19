
function [gP, vppP] = dataToGdx(tsoComm,hStart,tStart,pGenRT,pReal,filename)
%% 
% Import necessary packages

    import GAMS.*
    import GAMSTransfer.*
    
    %% 
    % Read data from Excel
    [busP, branchP, dispatchP, gP, tradeP, vppP] = readexcel(filename);

    % Prepare data
    [busPar, branchPar, dispatchPar, globalPar, gPar, realTimePar, tradePar, vppPar] = ...
                        prepare_data(busP, branchP, dispatchP, gP, hStart, pGenRT, tradeP, tStart, vppP);

    
    %% 
    % create an empty container and add data
    
    m = Container();
    
    % add global parameters
    
    period = Parameter(m, 'sPeriod', 'description', ...
        'Number of periods used in energy markets [hr]');
    period.setRecords(double(string(globalPar.value(1))));
    
    subPeriod = Parameter(m, 'sSubPeriod', 'description', ...
        'Number of subdivisions for real time optimization');
    subPeriod.setRecords(double(string(globalPar.value(2))));
    
    hSstart = Parameter(m, 'sPstart', 'description', 'Starting period for optimization');
    hSstart.setRecords(hStart);
    
    pStart = Parameter(m, 'sSubStart', 'description', 'Starting subperiod for optimization');
    pStart.setRecords(tStart);
    
    tsoCommand = Parameter(m, 'sTsoCommand', 'description', 'TSO command for set point changes');
    tsoCommand.setRecords(tsoComm);

    realDel = Parameter(m, 'pRealDel', 'description', 'Real-time delivery');
    realDel.setRecords(pReal);

    firstOrderLevel = 0.6;
    firstOrderMul = Parameter(m, 'sFirstOrderMul', 'description', ...
        'Multiplier to check if unit response is beyond expected level');
    firstOrderMul.setRecords(firstOrderLevel);
    
    
    % add sets
    
    b = Set(m, 'b', 'records', {unique(busP.ID)},       'description', 'Buses');
    v = Set(m, 'v', 'records', {unique(vppP.ID)},       'description', 'VPP units');
    x = Set(m, 'x', 'records', {unique(busP.BusType)},  'description', 'Bus types');
    y = Set(m, 'y', 'records', {unique(vppP.UnitType)}, 'description', 'VPP technology');
    
    bx = Set(m, 'bx', {b,x}, 'description', 'Buses and bus type');
    bx.setRecords(busP.ID,busP.BusType);
    
    vb = Set(m, 'vb', {v,b}, 'description', 'VPP units and associated bus');
    vb.setRecords(vppP.ID,vppP.BusNumber);
    
    vy = Set(m, 'vy', {v,y}, 'description', 'VPP and VPP technology');
    vy.setRecords(vppP.ID,vppP.UnitType);
    
    
    % add other parameters as tables
    
    % Branch parameters
    pBranchData = Parameter(m, 'pBranchData', {'*','*','*'}, 'description', ...
        'Branch parameters', 'domain_forwarding', true);
    pBranchData.setRecords(branchPar);
    
    % Bus parameters
    pBusData = Parameter(m, 'pBusData', {'*', '*'}, 'description', ...
        'Bus parameters', 'domain_forwarding', true);
    pBusData.setRecords(busPar);     
    
    % Dispatch parameters
    pDispatchData = Parameter(m, 'pDispatchData', {'*','*','*'}, 'description', ...
        'Dispatch parameters', 'domain_forwarding', true);
    pDispatchData.setRecords(dispatchPar);

    % Real-Time delivery parameters
    pRealTimeData = Parameter(m, 'pRealTimeData', {'*','*','*'}, 'description', ...
        'Real-Time delivery parameters', 'domain_forwarding', true);
    pRealTimeData.setRecords(realTimePar);
    
    % Global parameters
    pGlobal = Parameter(m, 'pGlobal', {'*'}, 'description', 'Global parameters', ...
        'domain_forwarding', true);
    pGlobal.setRecords(gPar);

    % Trade parameters
    pTradeData = Parameter(m, 'pTradeData', {'*','*','*'}, 'description', ...
        'Trade parameters', 'domain_forwarding', true);
    pTradeData.setRecords(tradePar);
    
    % VPP unit parameters
    pVppData = Parameter(m, 'pVppData', {'*', '*'}, 'description', ...
        'VPP unit parameters', 'domain_forwarding', true);
    pVppData.setRecords(vppPar);    
    
     
    %%
    % Write data to a GDX file
    
    m.write('Redisp.gdx');

end
%% 


function [busP, branchP, dispatchP, gP, tradeP, vppP] = readexcel(filename)
 
    % Set read options

    opts  = detectImportOptions(filename); % Initial detection  

    opts.VariableNamesRange = 'A15';
    opts.DataRange          = 'A16';    

    % Read data
    
    busP       = readtable(filename, opts, 'Sheet', 'Buses');
    busP       = busP(:,1:9);

    branchP    = readtable(filename, opts, 'Sheet', 'Branches_Lines');
    branchP    = branchP(:,1:9);

    dispatchP  = readtable(filename, opts, 'Sheet', 'Dispatches');
    dispatchP  = dispatchP(:,1:12);

    gP         = readtable(filename, opts, 'Sheet', 'Global_params'); 
    gP         = gP(:,1:15);

    tradeP     = readtable(filename, opts, 'Sheet', 'Bus_trade');
    tradeP     = tradeP(:,1:7);
    
    vppP       = readtable(filename, opts, 'Sheet', 'VPP_Units');

end
%% 


function [busPar, branchPar, dispatchPar, globalPar, gPar, realTimePar, tradePar, vppPar] = ...
                        prepare_data(busP, branchP, dispatchP, gP, hStart, pGenRT, tradeP, tStart, vppP)

    % Global Parameters
    globalP         = gP(:,1:15);
    globalPVarNames = globalP.Properties.VariableNames;
    globalPData     = table2array(convertvars(globalP,globalPVarNames,'categorical'));
    globalPVarNames = categorical(globalPVarNames);
    globalPar       = table(globalPVarNames', globalPData');
    globalPar.Properties.VariableNames(1) = "uni_1";
    globalPar.Properties.VariableNames(2) = "value";

    gPar1   = table2array(globalPar(:,1));
    gPar2   = table2array(globalPar(:,2));
    gPar3   = double(string(gPar2));
    gPar    = table(gPar1,gPar3);
    gPar.Properties.VariableNames(1) = "uni_1";
    gPar.Properties.VariableNames(2) = "value";

    %% 
    % VPP Parameters
    [vppPar] = makeGdxTable3Cols(vppP);

    %% 
    % Bus Parameters
    [busPar] = makeGdxTable3Cols(busP);

    %% 
    % Branch parameters
    branFrom     = categorical(branchP.FromBusNumber);
    branTo       = categorical(branchP.ToBusNumber);
    branVarNames = categorical(branchP.Properties.VariableNames);
    branVarNames = branVarNames(:,3:end);
    
    [a,~] = size(branFrom);
    [~,b] = size(branVarNames);
    
    firstCol  = [];
    secondCol = [];
    thirdCol  = [];
    fourthCol = [];
    
    for i = 1:a
        for j = 1:b
            firstCol   = [firstCol; branFrom(i)];
            secondCol  = [secondCol; branTo(i)];
            thirdCol   = [thirdCol; branVarNames(j)];
            fourthCol  = [fourthCol; table2array(branchP(i,j+2))];        
        end
    end
    
    branchPar = table(firstCol, secondCol, thirdCol, double(string(fourthCol)));
    branchPar.Properties.VariableNames(1) = "uni_1";
    branchPar.Properties.VariableNames(2) = "uni_2";
    branchPar.Properties.VariableNames(3) = "uni_3";
    branchPar.Properties.VariableNames(4) = "value";

    %% 
    % Dispatch Parameters       
    [dispatchPar] = makeGdxTable4Cols(dispatchP);

    % Trade Parameters       
    [tradePar] = makeGdxTable4Cols(tradeP);

    % Real time delivery parameters
    units = ["WPP", "F-DEM", "STU", "PV"];
    iD    = categorical(cellstr(units));
    megaWatt = 1e6;
        
    [~,a] = size(iD);
    
    firstCol  = [];
    secondCol = [];
    thirdCol  = [];
    fourthCol = [];
    
    for i = 1:a
        firstCol   = [firstCol;  iD(i)];
        secondCol  = [secondCol; categorical(hStart)];
        thirdCol   = [thirdCol;  categorical(tStart)];
        fourthCol  = [fourthCol; pGenRT(i+1)/megaWatt];        
    end
    
    realTimePar = table(firstCol, secondCol, thirdCol, double(string(fourthCol)));
    realTimePar.Properties.VariableNames(1) = "uni_1";
    realTimePar.Properties.VariableNames(2) = "uni_2";
    realTimePar.Properties.VariableNames(3) = "uni_3";
    realTimePar.Properties.VariableNames(4) = "value";
    
end
%% 


function tab = makeGdxTable3Cols(fullTable)
    iD       = categorical(fullTable.ID);
    varNames = categorical(fullTable.Properties.VariableNames);
    varNames = varNames(:,2:end);
    
    [a,~] = size(iD);
    [~,b] = size(varNames);
    
    firstCol  = [];
    secondCol = [];
    thirdCol  = [];
    
    for i = 1:a
        for j = 1:b
            firstCol  = [firstCol ; iD(i)              ];
            secondCol = [secondCol; varNames(j)        ];
            thirdCol  = [thirdCol ; table2array(fullTable(i,j+1))];        
        end
    end
    
    tab = table(firstCol, secondCol, double(string(thirdCol)));
    tab.Properties.VariableNames(1) = "uni_1";
    tab.Properties.VariableNames(2) = "uni_2";
    tab.Properties.VariableNames(3) = "value";

end
%% 


function tab = makeGdxTable4Cols(fullTable)

    iD       = categorical(fullTable.ID);
    period   = categorical(fullTable.Period);
    varNames = categorical(fullTable.Properties.VariableNames);
    varNames = varNames(:,3:end);
    
    fullTable.PAvailable = double(string(fullTable.PAvailable));
    fullTable.QLoad      = double(string(fullTable.QLoad));
    
    [a,~] = size(iD);
    [~,b] = size(varNames);
    
    firstCol  = [];
    secondCol = [];
    thirdCol  = [];
    fourthCol = [];
    
    for i = 1:a
        for j = 1:b
            firstCol   = [firstCol;  iD(i)];
            secondCol  = [secondCol; period(i)];
            thirdCol   = [thirdCol;  varNames(j)];
            fourthCol  = [fourthCol; table2array(fullTable(i,j+2))];        
        end
    end
    
    tab = table(firstCol, secondCol, thirdCol, double(string(fourthCol)));
    tab.Properties.VariableNames(1) = "uni_1";
    tab.Properties.VariableNames(2) = "uni_2";
    tab.Properties.VariableNames(3) = "uni_3";
    tab.Properties.VariableNames(4) = "value";

end
%% 

















