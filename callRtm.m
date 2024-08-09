
function [pGen,pReg,obj,timeDiv,solverTime] = callRtm(tsoComm,hStart,tStart,pGenRT,filename)

%     tic
    % Import necessary packages
    
    import GAMS.*
    import GAMSTransfer.*
    
    %%
    % Prepare data and write into a gdx file
    [globalPars, vppPars] = dataToGdx(tsoComm,hStart,tStart,pGenRT,filename);
    
    %%
    % Call GAMs solver from the MATLAB environment
    
    strgamsexe  = '"C:\GAMS\39\gams.exe" ';
    strgamsfile = '"./MainRtm.gms" ';
    strgamsout  = 'lo=3 gdx=aFRR';
    
    system([strgamsexe, strgamsfile, strgamsout],'-echo')
    
    %% 
    % Results of the real-time VPP operation result for secondary reserve provision 

    gdxFilename = 'aFRR.gdx';
    [pGen,pReg,obj,timeDiv] = gdxToTable(gdxFilename,vppPars,globalPars);

%     toc

    solverTime = toc-tic;

end




