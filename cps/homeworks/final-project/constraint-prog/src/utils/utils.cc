/**
 *
 * This class continas utilities shuch as printing debug info, preparing output, showing help, etc.
 *
 * @author: Juan Pablo Royo Sales
 * @date: April 8th, 2020
 * @subject: CPS
 * @institution: Universitat Politècnica Catalunya
 * @title: Final Project
 *
 */


#include <cstdlib>

#ifndef BOX_WRAPPER
#define BOX_WRAPPER
#include "../domain/box_wrapper.cc"
#endif

#ifndef BOX_SPACE
#define BOX_SPACE
#include "../space/box_space.cc"
#endif

using namespace std;

namespace utils {

  void show_help(int argc, char* argv[]){
      // Write help message.
    if (argc != 1 and (string(argv[1]) == "-h" or string(argv[1]) == "--help")) {
      cerr << "Run the Box Wrapping Constrain Programming Problem" << endl;
      cerr << "Usage: " << argv[0] << " < bwp_W_N_k.CP.inp" << endl;
      exit(0);
    }
  }

  void show_result(Boxes boxes, BoxWrapping* result){
    boxes.show();
    result->show();
  }

}
