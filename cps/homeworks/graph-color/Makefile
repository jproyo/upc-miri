CXX  = g++ -std=c++11 -F/Library/Frameworks
DIR  = /usr/local
LIBS = -framework gecode

graph-color: graph-color.cpp
	$(CXX) -I$(DIR)/include -c graph-color.cpp
	$(CXX) -L$(DIR)/lib -o graph-color graph-color.o $(LIBS)



