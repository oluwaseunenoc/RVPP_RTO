# RVPP_RTO
Tool for real-time operation of Renewable based Virtual Power Plants.

The dynamic model is implemented in RT-LAB, the OPAL-RT simulation framework. 
Within the model, the RVPP parameters from MS-Excel are obtained and utilized in the following ways: i) power set points are used for initialization of the dynamic model and ii) conversion to a .gdx file extension usable by the GAMS optimization environment is done. 

The input parameter files describe the RVPP, the cleared market schedules, available reserve, network topology and technical parameters of all units managed by the RVPP. Redispatch_Scenario1.xlsx contains the main parameters for results presented in Section IV-B. The other files with .xlsx extension are the IEEE standard test parameters.

Thereafter, periodic calls are made to AHORA (MainRtm.gms written in GAMS) during operation. 
When changes occur in the system and re-dispatch of units is required, the necessary input parameters are passed to GAMS for real-time re-dispatch, the simulation environment reads and processes the output and applies them to the RVPP units as updated set points.
