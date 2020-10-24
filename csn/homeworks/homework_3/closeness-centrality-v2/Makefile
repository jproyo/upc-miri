DIR = /usr/local
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  CCFLAGS += -D OSX
	CXX  = g++ -std=c++11 -F/Library/Frameworks
	LIBS = 
else
	CXX = g++ -std =c ++11
	LIBS = 
endif


all: main
.PHONY: all

clean:
	rm -rf bin/*

bin/main.o: src/main.cc
	$(CXX) -I$(DIR)/include -c src/domain/graph.cc -o bin/main.o

main: bin/main.o
	$(CXX) -L$(DIR)/lib -o bin/closeness bin/main.o $(LIBS)



