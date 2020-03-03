CXX  = g++ -std=c++11 -F/Library/Frameworks
DIR  = /usr/local
LIBS = -framework gecode

send-more-money: send-more-money.cpp
	$(CXX) -I$(DIR)/include -c send-more-money.cpp
	$(CXX) -L$(DIR)/lib -o send-more-money send-more-money.o $(LIBS)



