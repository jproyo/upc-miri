CXX  = g++ -std=c++11 -F/Library/Frameworks
DIR  = /usr/local
LIBS = -framework gecode

queens: queens.cpp
	$(CXX) -I$(DIR)/include -c queens.cpp
	$(CXX) -L$(DIR)/lib -o queens queens.o $(LIBS)



