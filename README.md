# RVPP_RTO
Tool for real-time operation of Renewable based Virtual Power Plants. Subsequently, the simulink file will be uploaded.

The dynamic model is implemented in RT-LAB, the OPAL-RT simulation framework. 
Within the model, the RVPP parameters from MS-Excel are obtained and utilized in the following ways: i) power set points are used for initialization of the dynamic model and ii) conversion to a .gdx file extension usable by the GAMS optimization environment is done. 

Thereafter, periodic calls are made to GAMS during operation. 
When changes occur in the system and re-dispatch of units is required, the necessary input parameters are passed to GAMS for real-time re-dispatch, the simulation environment reads and processes the output and applies them to the RVPP units as updated set points
