/**
 *
 * This class contains the main entry point of the program and the Space subclass with constraint
 *
 * @author: Juan Pablo Royo Sales
 * @date: April 6th, 2020
 * @subject: CPS
 * @institution: Universitat Politècnica Catalunya
 * @title: Final Project
 *
 */

#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>
#include <vector>

#ifndef UTILS
#define UTILS
#include "utils/utils.cc"
#endif


#ifndef BOX_SPACE
#define BOX_SPACE
#include "../space/box_space.cc"
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
    }

    show_result(boxes, newSol);

    delete newSol;

  }catch(Exception e) {
    cerr << "Exception on Program: " << e.what() << endl;
    return 1;
  }
}
