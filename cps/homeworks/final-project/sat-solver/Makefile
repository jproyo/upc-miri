
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

CCFLAGS = $(CCOPT) -I$(CPLEXINCDIR) -I$(CONCERTINCDIR)


BIN_DIR = bin
SRC_DIR = src

all: checker main
.PHONY: all

clean:
	rm -rf $(BIN_DIR)/*

main.o:
	$(CCC) $(CCFLAGS) -c $(SRC_DIR)/main.cc -o $(BIN_DIR)/boxw.o

main: main.o
	$(CCC) $(CCFLAGS) $(BIN_DIR)/boxw.o -o $(BIN_DIR)/boxw $(CCLNFLAGS)

checker.o:
	$(CCC) $(CCFLAGS) -c $(SRC_DIR)/checker.cc -o $(BIN_DIR)/checker.o

checker: checker.o
	$(CCC) $(CCFLAGS) $(BIN_DIR)/checker.o -o $(BIN_DIR)/checker

