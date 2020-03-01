/*********************************************
 * OPL 12.8.0.0 Model
 * Author: Juan Pablo Royo Sales
 * Creation Date: Sep 30, 2019 at 10:11:42 AM
 *********************************************/

 
int nTasks=...;
int nCPUs=...;

range T=1..nTasks;
range C=1..nCPUs;

float rt[t in T]=...;
float rc[c in C]=...;

dvar boolean x_tc[t in T,c in C];
dvar float+ z;

execute {
 var totalLoad = 0;
 
 for(var i=1;i<=nTasks;i++)
 	totalLoad += rt[i];
 	
 writeln("Total load requested resources: "+totalLoad);
 
};

minimize z;

subject to{

  assignments: 
  	forall(t in T) 
  		sum(c in C) x_tc[t,c] == 1;
  			      
  capacity: 
  	forall(c in C) 
  		sum(t in T) rt[t] * x_tc[t,c] <= rc[c];
               
  highest_load: 
  	forall(c in C) 
  		z >= (1/rc[c]) * sum(t in T) rt[t] * x_tc[t,c];

}
                   

execute {
 for(var i=1;i<=nCPUs;i++){
 	var load = 0;
 	for(var j=1;j<=nTasks;j++)
 		load += (rt[j] * x_tc[j][i]);
 	load = (1/rc[i])*load;
 	writeln("CPU "+i+" loaded at "+load*100+" %");
} 
};