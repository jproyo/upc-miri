#include <list> 
#include <set>
#include <map>
#include <iomanip>
#include <string>
#include <igraph.h>

#ifndef GRAPH
#define GRAPH
#include "../domain/graph.cc"
#endif

using namespace std;

class Montecarlo {

    private:
        Graph graph;
        int T;
        double x;

    Montecarlo(const Graph& g, int _T, double _x): 
    graph(g), x(_x), T(_T) {}

    double calcPValue(){
        int countSuccess = 0;
        double newCloseness = 0.0;
        for(int i = 0; i < T; i++){
            Graph g2 = produceNewGraph();
            if(g2.ClosenessCentralityReduced() >= this->x){
                countSuccess++;
            }
        }
        return countSuccess/(1.0*T);
    }

    Graph produceNewGraph(){
        igraph_integer_t diameter;
        igraph_t graph;
        igraph_rng_seed(igraph_rng_default(), 42);
        igraph_erdos_renyi_game(&graph, IGRAPH_ERDOS_RENYI_GNM, this->graph.V, this->graph.E,
                                 IGRAPH_UNDIRECTED, IGRAPH_NO_LOOPS);
        igraph_destroy(&graph);
    }
};