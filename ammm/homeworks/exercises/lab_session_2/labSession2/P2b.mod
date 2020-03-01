
 int nTasks=...;
 int nCPUs=...;
 
 range T=1..nTasks;
 range C=1..nCPUs;
 
 float rt[t in T]=...;
 float rc[c in C]=...;
 
 dvar boolean x_tc[t in T, c in C];
 
 execute {
	var totalLoad=0;
	for (var t=1;t<=nTasks;t++)
		totalLoad += rt[t];
	writeln("Total load "+ totalLoad);
	
	var totalResources = 0;
	for (var c=1;c<=nCPUs;c++)
		totalResources += rc[c];
		
	if (totalLoad > totalResources) {
		writeln("Load cannot be served");
		stop();		
	}
	
 };
 
 // Objective (note that minimizing the amount of non-served load is the same
 // as maximizing the amount of served load)
 maximize sum(t in T, c in C) rt[t]*x_tc[t,c];
 
 subject to{
	 // Constraint 1 (modified in order to allow taks to be rejected)
 		forall(t in T)
 			sum(c in C) x_tc[t,c] <= 1;
 	
 	// Constraint 2
 		forall(c in C)
 			sum(t in T) rt[t]* x_tc[t,c] <= rc[c];
 			
 			
}


execute {

	for (var c=1;c<=nCPUs;c++) {
		var load=0;
		for (var t=1;t<=nTasks;t++)
			load+=(rt[t]* x_tc[t][c]);
		load = (1/rc[c])*load;
		writeln("CPU " + c + " loaded at " + load + "%");
	}
	
};