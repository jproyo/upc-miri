/*********************************************
 * OPL 12.6.0.0 Model
 * Author: oliveras
 * Creation Date: Apr 5, 2019 at 10:47:26 AM
 *********************************************/

 int nClasses = ...;
 int nOptions = ...;
 range O = 1..nOptions;
 range C = 1..nClasses;
 
 int carsOfClass[c in C] = ...;
 int classOption[c in C][o in O] = ...;              
 int m[o in O] = ...;
 int k[o in O] = ...;

 int nPositions; 
 execute {
 	nPositions = 0;
 	for (var c = 1; c <= nClasses; ++c)
 		nPositions += carsOfClass[c];
 }
 
 range P = 1..nPositions; 
 // IMPORTANT HINT!!!!!!
 // A position p in P is a possible starting point of a window for option o iff p + k[o] - 1 <= nPositions.
 // The window itself is [ p...(p+k[o]-1) ]
 
 dvar boolean pc[p in P][c in C];
 dvar boolean po[p in P][o in O];
 
// MODEL B:
minimize sum(c in C, p in P : 2 <= p <= nPositions - 1) ((pc[p][c] != pc[p-1][c]) && (pc[p][c] != pc[p+1][c]) && (pc[p][c] == 1));
  
 subject to {
 	// MODEL A:
 	
 	// First constraint. Only one class in each pipe stage  
 	forall(p in P)
  		sum (c in C) pc[p][c] == 1;
 	  
 	// No more than m[o] elements in window k[o] for option "o" in O
 	forall (o in O) 
		forall (p in P)
		  	if (p + k[o] - 1 <= nPositions) // Equivalente a forall (p in P : (p + k[o] - 1 <= nPositions))
    			sum (i in p..(p + k[o] - 1)) po[i,o] <= m[o];
 	    
 	// Limited number of cars based on carsOfClass
 	forall (c in C)
		sum(p in P) pc[p][c] == carsOfClass[c];
	
	// Connect po with pc	
 	forall (p in P)
  		forall (o in O)
    		sum (c in C) (po[p][o] == classOption[c][o] && pc[p][c] == 1) == 1;
    		
    
 }
 
 execute {
  	var solution = new Array(1+nPositions);
 	write("SEQUENCE OF CLASSES: ");
 	for (var p = 1; p <= nPositions; ++p) {
 		var placed = 0; 
 		var cl;
 		for (var c = 1; c <= nClasses; ++c) if (pc[p][c] == 1) {++placed; cl = c;}
 		if (placed == 0) {writeln("ERROR: In position " + p + " there is no car"); stop();}
 		else if (placed > 1) {writeln("ERROR: In position " + p + " there is more than one car");stop();}
 		else {solution[p] = cl; write(cl + " ");} 		 
 	}
 	writeln();writeln();
 	for (var o = 1; o <= nOptions; ++o) {
 	 	
 		var violations = 0;
 	 	var solOpt = new Array(1+nPositions);
 		write("OPTION " + o + ":            ");
 		for (var p = 1; p <= nPositions; ++p) { 		 	 	
 			if (classOption[solution[p]][o] == 1) {write("X "); solOpt[p] = 1;}
 			else {write(". "); solOpt[p] = 0;}
        }    		
 		
 		for (p = 1; p + k[o] - 1 <= nPositions; ++p) {
 			placed = 0;
 			for (var pp = p; pp <= p + k[o] - 1; ++pp) {
 				if (solOpt[pp] == 1) ++placed;
  			} 			 			
 			if (placed > m[o]) { 			 			
 				if (violations == 0) write("\tViolations in windows: ");
 				++violations;
 				write("[" + p  + "..." + (p + k[o] - 1) + "] ");
   			} 				 			 		 		
 		}
 		
 		writeln();
 	} 		 		 	
 	
 }  
 
 