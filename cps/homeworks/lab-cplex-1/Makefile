
################################

# Uncomment for CPLEX_Enterprise_Server126
# SYSTEM     = x86-64_linux
# LIBFORMAT  = static_pic
# CPLEXDIR      = /opt/ibm/ILOG/CPLEX_Enterprise_Server126/CPLEX_Studio/cplex
# CONCERTDIR    = /opt/ibm/ILOG/CPLEX_Enterprise_Server126/CPLEX_Studio/concert

# Uncomment for CPLEX_Studio124

CPLEX_STUDIO_DIR = /Applications/CPLEX_Studio1210
#SYSTEM     = x86-64_sles10_4.1
SYSTEM           = x86-64_osx
LIBFORMAT        = static_pic
CPLEXDIR         = $(CPLEX_STUDIO_DIR)/cplex
CONCERTDIR       = $(CPLEX_STUDIO_DIR)/concert

################

CCC = g++

CCOPT = -m64 -O3 -fPIC -fno-strict-aliasing -fexceptions -DNDEBUG -DIL_STD
#CCOPT = -m64 -O3 -fPIC -fno-strict-aliasing -fexceptions -DIL_STD

CPLEXBINDIR   = $(CPLEXDIR)/bin/$(BINDIST)
CPLEXLIBDIR   = $(CPLEXDIR)/lib/$(SYSTEM)/$(LIBFORMAT)
CONCERTLIBDIR = $(CONCERTDIR)/lib/$(SYSTEM)/$(LIBFORMAT)

CCLNFLAGS = -L$(CPLEXLIBDIR) -lilocplex -lcplex -L$(CONCERTLIBDIR) -lconcert -lm -pthread -ldl

# -ldl needed for CPLEX version > 12.6


CONCERTINCDIR = $(CONCERTDIR)/include
CPLEXINCDIR   = $(CPLEXDIR)/include

CCFLAGS = $(CCOPT) -I$(CPLEXINCDIR) -I$(CONCERTINCDIR) -g

BIN_DIR = bin
SRC_DIR = src

all: ex1
.PHONY: all

clean:
	rm -rf $(BIN_DIR)/*

ex1: ex1.o
	$(CCC) $(CCFLAGS) $(BIN_DIR)/ex1.o -o $(BIN_DIR)/ex1 $(CCLNFLAGS)

ex1.o:
	$(CCC) -c $(CCFLAGS) $(SRC_DIR)/ex1.cpp -o $(BIN_DIR)/ex1.o


