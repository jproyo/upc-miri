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
        double x;
        int model;

    public: 
    Montecarlo(const Graph& g, int _T, int _Q, int _model, double _x): 
    graph(g), x(_x), T(_T), Q(_Q), model(_model){}

    double CalcPValue(){
        int countSuccess = 0;
        double newCloseness = 0.0;
        for(int i = 0; i < T; i++){
            if(model == 1){
                Graph newG = DoSwitching();
                double closeness = newG.ClosenessCentrality();
                cout << "Closeness c_mix_hnull " << fixed << setprecision(7) << closeness << endl;
                if(closeness >= this->x){
                    countSuccess++;
                }
            }else{
                Graph newG = DoBinomial(graph.GetV(), graph.GetE());
                double closeness = newG.ClosenessCentrality();
                cout << "Closeness c_mix_hnull " << fixed << setprecision(7) << closeness << endl;
                if(closeness >= this->x){
                    countSuccess++;
                }
            }
        }
        return countSuccess/(1.0*T);
    }

    private:

    Graph DoSwitching(){
        Graph newG = Graph(graph);
        for(int i = 0;i<Q*newG.GetE();i++){
            int randomVertex1 = ((double) rand() / (RAND_MAX)) * newG.GetV();
            int randomVertex2 = ((double) rand() / (RAND_MAX)) * newG.GetV();
            if(randomVertex1 != randomVertex2){
                newG.TrySwitch(randomVertex1, randomVertex2);
            }
        }
        return newG;
    }

    // A function to generate random graph.
    Graph DoBinomial(int V, int E)
    {
        Graph g(graph.GetLanguage(), V);
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

        for(int i=0; i<E;i++){
            g.addEdge(edge[i][0], edge[i+1][1]);
            g.addEdge(edge[i+1][1], edge[i][0]);
        }
        return g;
    }
};