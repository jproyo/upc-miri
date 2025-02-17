/**
 *
 * Main for calculting Aproximation on switching and binomial
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


#ifndef GRAPH
#define GRAPH
#include "domain/graph.cc"
#endif

#ifndef APROX
#define APROX
#include "graph/aprox.cc"
#endif

using namespace std;
using namespace utils;

int main(int argc, char* argv[]) {

  show_help_aprox(argc,argv);

  string path = argv[1];
  int t = atoi(argv[2]);
  int q = atoi(argv[3]);
  int model = atoi(argv[4]);
  Graph g = Graph::fromStdIn(path);
  Montecarlo m = Montecarlo(g,t,q, model);
  cout << "Executing Montecarlo for Language " << g.GetLanguage() << " - T " << t << " - Q " << q << " - Model " << (model == 1?"Switching":"Binomial") << endl;
  chrono::steady_clock::time_point begin = chrono::steady_clock::now();
  double pval = m.CalcPValue();
  chrono::steady_clock::time_point end = chrono::steady_clock::now();
  cout << "PValue for Language " << g.GetLanguage() << " " << fixed << std::setprecision(7) << pval << endl;
  cout << "Elapsed time: " << fixed << std::setprecision(2) << (chrono::duration_cast<std::chrono::microseconds>(end - begin).count()) << " microsec " << endl;

  return 0; 

}
