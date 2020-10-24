DIR = /usr/local
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Darwin)
  CCFLAGS += -D OSX
	CXX  = g++ -std=c++11 -F/Library/Frameworks
	LIBS = -ligraph
else
	CXX = g++ -std =c ++11
	LIBS = -ligraph
endif


all: main
.PHONY: all

clean:
	rm -rf bin/*

bin/aprox.o: src/aprox.cc
	$(CXX) -I$(DIR)/include -c src/graph/aprox.cc -o bin/aprox.o

bin/main.o: src/main.cc
	$(CXX) -I$(DIR)/include -c src/domain/graph.cc -o bin/main.o

main: bin/main.o bin/aprox.o
	$(CXX) -L$(DIR)/lib -o bin/closeness bin/main.o $(LIBS)



