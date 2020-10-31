/**
 *
 * Class that represent Graph
 *
 * @author: Juan Pablo Royo Sales
 * @date: October 21, 2020
 * @subject: CSN
 * @institution: Universitat Politècnica Catalunya
 * @title: Homework 3
 *
 */
#include <iostream> 
#include <fstream>
#include <filesystem>
#include <list> 
#include <set>
#include <map>
#include <iomanip>
#include <regex>
#include <string>
#include <chrono>
#include <vector>
#include <queue>
#include <numeric>      // std::iota
#include <algorithm>    // std::sort, std::stable_sort

namespace fs = std::__fs::filesystem;

using namespace std;

vector<size_t> sort_indexes(const vector<int> *v, int size) {
  vector<size_t> idx(size);
  iota(idx.begin(), idx.end(), 0);
  stable_sort(idx.begin(), idx.end(),
       [&v](size_t i1, size_t i2) {return v[i1].size() >= v[i2].size();});

  return idx;
}

class Graph 
{ 
    string language;
    int V;
    int E;
    vector<int> *adj; 

public: 
	Graph(string language, int V); 
    Graph(const Graph& g2);
    Graph();

	void addEdge(int v, int w); 

    double ClosenessCentrality(); 

    double ClosenessCentralityReduced(int M);

    void TrySwitch(int v1, int v2);

    void PrintAdjList();

    void PrintTable1Report();

    string GetLanguage();

    int GetV();

    int GetE();

    vector<int>* GetAdj();

    double GetK();

    double GetDelta();

    void SetEdgesCount(int count);

    static Graph fromStdIn(string filepath){
        int vertices;
        int edges;
        std::regex rgx(".*data\\/(\\w+)_.*");
        std::smatch match;
        string language = "unknown";
        const string toMatch = filepath;
        if (std::regex_search(toMatch.begin(), toMatch.end(), match, rgx)){
            language = match[1];
        }
        ifstream inFile;
        inFile.open(filepath);
        if (inFile.is_open()){
            inFile >> vertices >> edges;
            Graph graph(language, vertices);
            string v1, v2;
            int c = 0;
            int v = 0;
            map<string, int> m;
            for(int i = 0; i<edges; i++){
                inFile >> v1 >> v2;
                if(v1.compare(v2) != 0) {
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
                    graph.addEdge(m[v1], m[v2]);
                    graph.addEdge(m[v2], m[v1]);
                    c++;
                }
            }
            inFile.close();
            graph.SetEdgesCount(c);
            return graph;
        }
        throw new exception;
    }

    private:

    double Closeness(int s, int **distances, vector<bool> distCalculated){
        if(adj[s].size() == 0) return 0.0;
        if(adj[s].size()<2){
            ClosenessLowDegree(s, distances, distCalculated);
        }else{
            ClosenessBFS(s, distances, distCalculated);
        }
        double closeness = 0.0;
        for(int i = 0; i<V; i++){
            if(distances[s][i] > 0) closeness += 1/(1.0*distances[s][i]);
        }
        return closeness/(1.0*(V-1));
    }

    void ClosenessLowDegree(int s, int **distances, vector<bool> distCalculated){
        if(!distCalculated.at(s)){
            int node = adj[s].front();
            distances[s] = new int[V];
            for(int i = 0; i<V; i++) distances[s][i] = 0;
            if(adj[node].size()<2 && adj[node].front() == s){
                distances[node] = new int[V];
                distances[s][node] = 1;
                distances[node][s] = 1;
                distCalculated.assign(node,
                true);        
            }else{
                CopyDistancesPlus1(node, s, distances);
            }
            distCalculated.assign(s, true);
        }
    }

    void CopyDistancesPlus1(int sourceNode, int targetNode, int **distances){
        for(int i = 0; i < V;i++){
            if(distances[sourceNode][i] != targetNode && distances[sourceNode][i] > 0){
                distances[targetNode][i] = distances[sourceNode][i] + 1;
            }
        }
        distances[targetNode][sourceNode] = 1;
    }

    void ClosenessBFS(int s, int **distances, vector<bool> distCalculated) 
    { 
        queue<int> q; 
        vector<bool> visited;
        visited.assign(V, false); 

        q.push(s);  
        distances[s] = new int[V];
        for(int i = 0; i<V; i++) distances[s][i] = 0;
        visited[s] = true; 
            
        while (!q.empty()) 
        { 
            int u = q.front(); 
            q.pop(); 

            for (auto i = adj[u].begin(); i != adj[u].end(); i++) 
            { 
                if (!visited[*i]) 
                { 
                    visited[*i] = true; 
                    distances[s][*i] = distances[s][u] + 1; 
                    q.push(*i);  
                } 
            } 
        } 
        distCalculated.assign(s, true);
    } 

}; 

Graph::Graph(){} 

Graph::Graph(string language, int V) 
{ 
    this->language = language;
	this->V = V; 
	adj = new vector<int>[V]; 
} 

Graph::Graph(const Graph &g){
    this->language = g.language;
    this->V = g.V;
    this->E = g.E;
    this->adj = new vector<int>[V];
    for (auto i = 0; i<V; i++){
        this->adj[i] = g.adj[i];
    }
}

void Graph::SetEdgesCount(int count){
    this->E = count;
}

void Graph::PrintAdjList(){
    for(int i = 0;i<V;i++){
        cout << "Vertex: " << i << "[";
        for(vector<int>::iterator iter = adj[i].begin(); iter != adj[i].end(); ++iter)
            cout << *iter << ((distance( iter, adj[i].end() ) == 1)  ? "" : ",");
        cout << "]" << endl;
    }
}

void Graph::addEdge(int v, int w) 
{ 
	adj[v].push_back(w); 
} 

string Graph::GetLanguage(){
    return this->language;
}

double Graph::GetK(){
    return (2*E)/(1.0*V);
}

double Graph::GetDelta(){
    return (2*E)/(1.0*V*(V-1));
}

int Graph::GetV(){
    return V;
}

int Graph::GetE(){
    return E;
}

vector<int>* Graph::GetAdj(){
    return this->adj;
}


void Graph::PrintTable1Report(){
    cout 
    << this->language << " & "
    << this->V << " & " 
    << this->E << " & " 
    << fixed << std::setprecision(7) 
    << this->GetK() << " & " 
    << fixed << std::setprecision(7) 
    << this->GetDelta() << " \\\\ " 
    << endl;
}

double Graph::ClosenessCentrality(){
    double closeness = 0.0;
    int **distances = new int*[V];
    vector<bool> distCalculated;
    distCalculated.assign(V, false);
    for (auto i: sort_indexes(adj, V)) {
        closeness += Closeness(i, distances, distCalculated);
    }
    delete *distances;
    return closeness/(1.0*V);
} 

double Graph::ClosenessCentralityReduced(int M){
    double closeness = 0.0;
    int **distances = new int*[V];
    vector<bool> distCalculated;
    distCalculated.assign(V, false);
    int a = 0;
    for (auto i: sort_indexes(adj, V)) {
        if(a >= M) break;
        closeness += Closeness(i, distances, distCalculated);
        a++;
    }
    return closeness/(1.0*V);
} 


void Graph::TrySwitch(int v1, int v2){
    vector<int> _v2 = adj[v2];
    vector<int> _v1 = adj[v1];
    auto onlyV1 = [&v2](const int &i){
        return i != v2;
    };
    auto onlyV2 = [&v1](const int &i){
        return i != v1;
    };
    sort(_v1.begin(), _v1.end());
    sort(_v2.begin(), _v2.end());
    std::vector<int> diff1;
    std::vector<int> diff2;
    std::set_difference(_v1.begin(), _v1.end(), _v2.begin(), _v2.end(), std::inserter(diff1, diff1.begin()));
    std::set_difference(_v2.begin(), _v2.end(), _v1.begin(), _v1.end(), std::inserter(diff2, diff2.begin()));
    std::vector<int>::iterator findDiff1 = std::find_if(diff1.begin(), diff1.end(), onlyV1);
    std::vector<int>::iterator findDiff2 = std::find_if(diff2.begin(), diff2.end(), onlyV2);
    if(findDiff1 != diff1.end() && findDiff2 != diff2.end()){
        int val_v1 = *findDiff1;
        int val_v2 = *findDiff2;
        std::vector<int>::iterator findPos1 = std::find(adj[v1].begin(), adj[v1].end(), val_v1);
        std::vector<int>::iterator findPos2 = std::find(adj[v2].begin(), adj[v2].end(), val_v2);
        if(findPos1 != adj[v1].end() && findPos2 != adj[v2].end()){
            int idx_1 = std::distance(adj[v1].begin(), findPos1);
            int idx_2 = std::distance(adj[v2].begin(), findPos2);
            adj[v1][idx_1] = val_v2;
            adj[v2][idx_2] = val_v1;
            std::vector<int>::iterator findOldPos1 = std::find(adj[val_v1].begin(), adj[val_v1].end(), v1);
            std::vector<int>::iterator findOldPos2 = std::find(adj[val_v2].begin(), adj[val_v2].end(), v2);
            if(findOldPos1 != adj[val_v1].end() && findOldPos2 != adj[val_v2].end()){
                int idx_old_1 = std::distance(adj[val_v1].begin(), findOldPos1);
                int idx_old_2 = std::distance(adj[val_v2].begin(), findOldPos2);
                adj[val_v1][idx_old_1] = v2;
                adj[val_v2][idx_old_2] = v1;
            }
        }
    }
}
