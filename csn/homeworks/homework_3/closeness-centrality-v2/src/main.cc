/**
 *
 * This class contains the main entry point of the program and the Space subclass with constraint
 *
 * @author: Juan Pablo Royo Sales
 * @date: October 2020
 * @subject: CSN
 * @institution: Universitat Politècnica Catalunya
 * @title: Closeness Centrality
 *
 */

#include <cstdlib>
#include <vector>

#ifndef UTILS
#define UTILS
#include "utils/utils.cc"
#endif


#ifndef DOMAIN
#define DOMAIN
#include "domain/graph.cc"
#endif

using namespace std;
using namespace utils;

int main(int argc, char* argv[]) {

  show_help(argc,argv);

  try{

    Boxes boxes = Boxes::fromStdIn();

    boxes.printDebug();

    BoxWrapping* m = new BoxWrapping(boxes);
    BAB<BoxWrapping> e(m);
    delete m;
    BoxWrapping* newSol;
    while(BoxWrapping* sol = e.next()) {
      if(sol) newSol = sol;
    }
    if(newSol) {
      newSol->printDebug();
      show_result(boxes, newSol);
    }else{
      return 1;
    }

    delete newSol;

  }catch(Exception e) {
    cerr << "Exception on Program: " << e.what() << endl;
    return 1;
  }
}
