CXX  = g++ -std=c++11 -F/Library/Frameworks
DIR  = /usr/local
LIBS = -framework gecode

all: checker roll-paper
.PHONY: all

clean:
	rm -rf bin/*

roll-paper: roll-paper.cc
	$(CXX) -I$(DIR)/include -c roll-paper.cc -o bin/roll-paper.o
	$(CXX) -L$(DIR)/lib -o bin/roll-paper bin/roll-paper.o $(LIBS)


checker: checker.cc
	$(CXX) -I$(DIR)/include -c checker.cc -o bin/checker.o
	$(CXX) -L$(DIR)/lib -o bin/checker bin/checker.o $(LIBS)


