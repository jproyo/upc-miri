CXX  = g++ -std=c++11 -F/Library/Frameworks
DIR  = /usr/local
LIBS = -framework gecode

queens: checker.cpp queens.cpp queens-2.cpp queens-3.cpp queens-4.cpp queens-5.cpp queens-6.cpp
	$(CXX) -I$(DIR)/include -c queens.cpp
	$(CXX) -L$(DIR)/lib -o bin/queens queens.o $(LIBS)

	$(CXX) -I$(DIR)/include -c queens-2.cpp
	$(CXX) -L$(DIR)/lib -o bin/queens-2 queens-2.o $(LIBS)

	$(CXX) -I$(DIR)/include -c queens-3.cpp
	$(CXX) -L$(DIR)/lib -o bin/queens-3 queens-3.o $(LIBS)

	$(CXX) -I$(DIR)/include -c queens-4.cpp
	$(CXX) -L$(DIR)/lib -o bin/queens-4 queens-4.o $(LIBS)

	$(CXX) -I$(DIR)/include -c queens-5.cpp
	$(CXX) -L$(DIR)/lib -o bin/queens-5 queens-5.o $(LIBS)

	$(CXX) -I$(DIR)/include -c queens-6.cpp
	$(CXX) -L$(DIR)/lib -o bin/queens-6 queens-6.o $(LIBS)

	$(CXX) -I$(DIR)/include -c checker.cpp
	$(CXX) -L$(DIR)/lib -o bin/checker checker.o $(LIBS)


