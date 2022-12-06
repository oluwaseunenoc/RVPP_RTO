
function [gg2,gg3,gg5,gg6,gg2Reg,gg3Reg,gg5Reg,gg6Reg,obj,timeDiv] = callRtm(tsoComm,hStart,tStart,filename)

    tic
    % Import necessary packages
    
    import GAMS.*
    import GAMSTransfer.*
    
    %%
    % Prepare data and write into a gdx file
    
    [globalPars, vppPars] = dataToGdx(tsoComm,hStart,tStart,filename);
    
    %%
    % Call GAMs solver from the MATLAB environment
    
    strgamsexe  = '"C:\GAMS\39\gams.exe" ';
    strgamsfile = '"C:\Users\oenoch\OPAL-RT\RT-LABv2022.1_Workspace\callGams_RTLAB\models\callGAMS\MainRtm.gms" ';
    strgamsout  = 'lo=3 gdx=aFRR';
    
    system([strgamsexe, strgamsfile, strgamsout],'-echo')
    
    %% 
    % Results of the real-time VPP operation result for secondary reserve provision 

    gdxFilename = 'aFRR.gdx';
    [gg2,gg3,gg5,gg6,gg2Reg,gg3Reg,gg5Reg,gg6Reg,obj,timeDiv] = gdxToTable(gdxFilename,vppPars,globalPars);

    toc

end




