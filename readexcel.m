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