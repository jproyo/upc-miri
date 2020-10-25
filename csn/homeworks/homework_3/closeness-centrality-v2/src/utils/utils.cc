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

using namespace std;

namespace utils {

  void show_help(int argc, char* argv[]){
      // Write help message.
    if (argc != 1 and (string(argv[1]) == "-h" or string(argv[1]) == "--help")) {
      cerr << "Run the Closeness Centrality Metric Significance" << endl;
      cerr << "Usage: " << argv[0] << " data/Some_Lang.txt" << endl;
      exit(0);
    }
  }
  void show_help_aprox(int argc, char* argv[]){
      // Write help message.
    if (argc != 4 and (string(argv[1]) == "-h" or string(argv[1]) == "--help")) {
      cerr << "Run the Closeness Centrality Metric Significance" << endl;
      cerr << "Usage: " << argv[0] << " \"data/Some_Lang.txt\" \"0.7\" 5 5" << endl;
      cerr << "1 parameter: Path to language file" << endl;
      cerr << "2 parameter: Closeness Centrality Real model" << endl;
      cerr << "3 parameter: T for Montecarlo" << endl;
      cerr << "4 parameter: Q for Switching" << endl;
      exit(0);
    }
  }

}
