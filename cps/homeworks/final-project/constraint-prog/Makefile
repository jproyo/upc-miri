DIR = /usr/local
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  CCFLAGS += -D OSX
	CXX  = g++ -std=c++11 -F/Library/Frameworks
	LIBS = -framework gecode
else
	CXX = g++ -std =c ++11
	LIBS = - lgecodedriver - lgecodesearch \
				 - lgecodeminimodel -lgecodeint \
				 - lgecodekernel - lgecodesupport
endif


all: checker main
.PHONY: all

clean:
	rm -rf bin/*

bin/main.o: src/main.cc
	$(CXX) -I$(DIR)/include -c src/main.cc -o bin/main.o

main: bin/main.o
	$(CXX) -L$(DIR)/lib -o bin/boxw bin/main.o $(LIBS)

bin/checker.o: src/checker.cc
	$(CXX) -I$(DIR)/include -c src/checker.cc -o bin/checker.o

checker: bin/checker.o
	$(CXX) -L$(DIR)/lib -o bin/checker bin/checker.o $(LIBS)


