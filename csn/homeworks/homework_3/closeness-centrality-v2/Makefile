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


all: main main_aprox
.PHONY: all

clean:
	rm -rf bin/*

bin/aprox.o: src/main_cc.cc src/main_aprox.cc
	$(CXX) -I$(DIR)/include -c src/graph/aprox.cc -o bin/aprox.o

bin/graph.o: src/main_cc.cc
	$(CXX) -I$(DIR)/include -c src/domain/graph.cc -o bin/graph.o

bin/main.o: src/main_cc.cc
	$(CXX) -I$(DIR)/include -c src/main_cc.cc -o bin/main.o

bin/main_aprox.o: src/main_aprox.cc
	$(CXX) -I$(DIR)/include -c src/main_aprox.cc -o bin/main_aprox.o

main: bin/main.o bin/graph.o bin/aprox.o
	$(CXX) -L$(DIR)/lib -o bin/closeness bin/main.o $(LIBS)

main_aprox: bin/main_aprox.o bin/graph.o bin/aprox.o
	$(CXX) -L$(DIR)/lib -o bin/montecarlo bin/main_aprox.o $(LIBS)


