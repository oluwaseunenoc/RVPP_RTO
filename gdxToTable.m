
function [pGen,pReg,obj,timeDiv] = gdxToTable(gdxFilename,vppPars,globalPars)
    
    s.name   = 'pReg';
    t.name   = 'pActivePower';
    obj.name = 'vCostRedispatch';

    pReg         = rgdx (gdxFilename, s);
    pActivePower = rgdx (gdxFilename, t);
    objValue     = rgdx (gdxFilename, obj);    
    
    % Obtain sorted VPP units
    vppUnits = sortrows(cell2table(vppPars.ID));
    
    % Obtain time division for power dispatch
    timeDiv = double(string(globalPars.SubPeriod));
    
    [a,~] = size(vppUnits);
    j = 0;
    powerReg      = zeros(timeDiv,a);
    powerDispatch = zeros(timeDiv,a);
    for i = 1:a
        powerReg(:,i)      = pReg.val(1+j*timeDiv:i*timeDiv,4);
        powerDispatch(:,i) = pActivePower.val(1+j*timeDiv:i*timeDiv,4);
        j=j+1;
    end

    g3  = powerDispatch(:,1); % Flexible demand
    g6  = powerDispatch(:,2); % PV
    g5  = powerDispatch(:,3); % STU
    g2  = powerDispatch(:,5); % WPP

    g3Reg  = powerReg(:,1);   % Flexible demand
    g6Reg  = powerReg(:,2);   % PV
    g5Reg  = powerReg(:,3);   % STU
    g2Reg  = powerReg(:,5);   % WPP

    pGen = {g2,g3,g5,g6};
    pReg = {g2Reg,g3Reg,g5Reg,g6Reg};
        
    obj = objValue.val;

end