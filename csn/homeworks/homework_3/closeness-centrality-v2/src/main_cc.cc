/**
 *
 * Main for calculting General Report. Table 1 and Closeness for table 2
 *
 * @author: Juan Pablo Royo Sales
 * @date: October 21, 2020
 * @subject: CSN
 * @institution: Universitat Politècnica Catalunya
 * @title: Homework 3
 *
 */
#include <cstdlib>
#include <vector>
#include <iostream>

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

    std::string path = argv[1];
    Graph g = Graph::fromStdIn(path);
    g.PrintTable1Report();
    // g.PrintAdjList();
    chrono::steady_clock::time_point begin = chrono::steady_clock::now();
    double closeness = g.ClosenessCentrality();
    chrono::steady_clock::time_point end = chrono::steady_clock::now();
    cout << "Closeness for Language " << g.GetLanguage() << " " << fixed << std::setprecision(7) << closeness << endl;
    cout << "Elapsed time: " << fixed << std::setprecision(2) << (chrono::duration_cast<std::chrono::microseconds>(end - begin).count()) << " microsec " << endl;

  	return 0; 

  }catch(exception e) {
    cerr << "Exception on Program: " << e.what() << endl;
    return 1;
  }
}
