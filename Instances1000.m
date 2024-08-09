
filenames=["Redispatch_scenario1.xlsx", "Redispatch_IEEE13.xlsx",...
           "Redispatch_IEEE34.xlsx", "Redispatch_IEEE123.xlsx"];

[~, sizeFilename] = size(filenames);

iterations = 1000;
results = zeros(iterations,1+sizeFilename);

hStart = 1;
tStart = 1;

tsoCommLower = -4;
tsoCommUpper = 4;

pGenRT = [0;0;0;0;0;0];


for f = 1:sizeFilename

    filename = filenames(f);
    if filename == "Redispatch_scenario1.xlsx"
        hStart = 16;
    else
        hStart = 1;
    end

    for iter = 1:iterations
            
        tsoComm = (tsoCommUpper-tsoCommLower).*rand(1,1) + tsoCommLower;

        tic;
        [~, ~, obj] = callRtm(tsoComm,hStart,tStart,pGenRT,filename);
        toc;

        controllerTime = toc;
        
        results(iter,1)    = iter;
        results(iter,f+1)  = controllerTime; 

    end
end

excelfilename = 'PF_Instances.xlsx';
writematrix(results,excelfilename,'Sheet','FULL_AC','Range','A2') % change sheet number for other PFs


