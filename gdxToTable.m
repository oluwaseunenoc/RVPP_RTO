
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

    g1  = powerDispatch(:,5); % WPP1 
    g2  = powerDispatch(:,3); % WPP2

    g1Reg  = powerReg(:,5);   % WPP1
    g2Reg  = powerReg(:,3);   % WPP2
    
    pGen = {g1,g2};
    pReg = {g1Reg,g2Reg};
        
    obj = objValue.val;

end