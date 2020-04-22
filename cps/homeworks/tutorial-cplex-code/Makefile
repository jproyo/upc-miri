
all: ex1 ex2

ex1: ex1.o
	$(CCC) $(CCFLAGS) ex1.o -o ex1 $(CCLNFLAGS)
ex1.o: ex1.cpp
	$(CCC) -c $(CCFLAGS) ex1.cpp -o ex1.o

ex2: ex2.o
	$(CCC) $(CCFLAGS) ex2.o -o ex2 $(CCLNFLAGS)
ex2.o: ex2.cpp
	$(CCC) -c $(CCFLAGS) ex2.cpp -o ex2.o

clean:
	rm --force ex1 ex1.o ex2 ex2.o

################################

# Uncomment for CPLEX_Enterprise_Server126
# SYSTEM     = x86-64_linux
# LIBFORMAT  = static_pic
# CPLEXDIR      = /opt/ibm/ILOG/CPLEX_Enterprise_Server126/CPLEX_Studio/cplex
# CONCERTDIR    = /opt/ibm/ILOG/CPLEX_Enterprise_Server126/CPLEX_Studio/concert

# Uncomment for CPLEX_Studio124
SYSTEM     = x86-64_sles10_4.1
LIBFORMAT  = static_pic
CPLEXDIR      = /opt/ibm/ILOG/CPLEX_Studio124/cplex
CONCERTDIR    = /opt/ibm/ILOG/CPLEX_Studio124/concert

################

CCC = g++

CCOPT = -m64 -O3 -fPIC -fno-strict-aliasing -fexceptions -DNDEBUG -DIL_STD

CPLEXBINDIR   = $(CPLEXDIR)/bin/$(BINDIST)
CPLEXLIBDIR   = $(CPLEXDIR)/lib/$(SYSTEM)/$(LIBFORMAT)
CONCERTLIBDIR = $(CONCERTDIR)/lib/$(SYSTEM)/$(LIBFORMAT)

CCLNFLAGS = -L$(CPLEXLIBDIR) -lilocplex -lcplex -L$(CONCERTLIBDIR) -lconcert -lm -pthread -ldl

# -ldl needed for CPLEX version > 12.6


CONCERTINCDIR = $(CONCERTDIR)/include
CPLEXINCDIR   = $(CPLEXDIR)/include

CCFLAGS = $(CCOPT) -I$(CPLEXINCDIR) -I$(CONCERTINCDIR) 
