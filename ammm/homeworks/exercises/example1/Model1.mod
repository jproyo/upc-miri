/*********************************************
 * OPL 12.8.0.0 Model
 * Author: juan
 * Creation Date: Sep 9, 2019 at 7:54:33 PM
 *********************************************/

 dvar float+ xa;
 dvar float+ xb;
 
 maximize 500*xa + 450*xb;
 
 subject to {
	 
	 time: 6*xa + 5*xb <= 60;
	 
	 storage: 10*xa + 20*xb <= 150;
	 
	 demand: xa <= 8;
	 
 }