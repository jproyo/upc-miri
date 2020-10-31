/**
 *
 * Aproximation Algorithm with Montecarlo using Binomial and Switching methods
 *
 * @author: Juan Pablo Royo Sales
 * @date: October 21, 2020
 * @subject: CSN
 * @institution: Universitat Politècnica Catalunya
 * @title: Homework 3
 *
 */
#include <list> 
#include <set>
#include <map>
#include <iomanip>
#include <string>

#ifndef GRAPH
#define GRAPH
#include "../domain/graph.cc"
#endif

using namespace std;

class Montecarlo {

    private:
        Graph graph;
        int T;
        int Q;
        int model;

    public: 
    Montecarlo(const Graph& g, int _T, int _Q, int _model): 
    graph(g), T(_T), Q(_Q), model(_model){}

    double CalcPValue(){
        int countSuccess = 0;
        int M = graph.GetV()/1000;
        double closenessOriginal = graph.ClosenessCentralityReduced(M); 
        for(int i = 0; i < T; i++){
            if(calcCC(closenessOriginal, M)) countSuccess++;
        }
        return countSuccess/(1.0*T);
    }

    private:

    bool calcCC(const double closenessOriginal, const int M){
        Graph* newG;
        if(model == 1){
            newG = DoSwitching();
        }else{
            newG = DoBinomial(graph.GetV(), graph.GetE());
        }
        double closeness = newG->ClosenessCentralityReduced(M);
        cout << "Closeness c_mix_hnull " << fixed << setprecision(7) << closeness << " - original "  << fixed << setprecision(7) << closenessOriginal << endl;
        return closeness >= closenessOriginal;
    }

    Graph* DoSwitching(){
        Graph* newG = new Graph(graph);
        for(int i = 0;i<Q*newG->GetE();i++){
            int randomVertex1 = ((double) rand() / (RAND_MAX)) * newG->GetV();
            int randomVertex2 = ((double) rand() / (RAND_MAX)) * newG->GetV();
            if(randomVertex1 != randomVertex2){
                newG->TrySwitch(randomVertex1, randomVertex2);
            }
        }
        return newG;
    }

    // A function to generate random graph.
    Graph* DoBinomial(int V, int E)
    {
        Graph* g = new Graph(graph.GetLanguage(), V);
        int i, j, edge[E][2];
    
        i = 0;
        // Build a connection between two random vertex.
        while(i < E)
        {
            edge[i][0] = (((double) rand() / (RAND_MAX)) * V) +1;
            edge[i][1] = (((double) rand() / (RAND_MAX)) * V) +1;
    
            if(edge[i][0] == edge[i][1])
                continue;
            else
            {
                for(j = 0; j < i; j++)
                {
                    if((edge[i][0] == edge[j][0] && edge[i][1] == edge[j][1]) || (edge[i][0] == edge[j][1] && edge[i][1] == edge[j][0]))
                        i--;
                }
            }
            i++;
        }

        int v = 0;
        map<int, int> m;
        for(int i=0; i<E;i++){
            int v1 = edge[i][0];
            int v2 = edge[i][1];
            if(v1 != v2){
                if ( m.find(v1) == m.end() ) {
                    if ( m.find(v2) == m.end() ) {
                        m[v1] = v++;
                        m[v2] = v++;
                    }else{
                        m[v1] = v++;
                    }
                }else{
                    if ( m.find(v2) == m.end() ) {
                        m[v2] = v++;
                    }
                }
                g->addEdge(m[v1], m[v2]);
                g->addEdge(m[v2], m[v1]);
            }
        }
        return g;
    }
};