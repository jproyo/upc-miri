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

namespace fs = std::__fs::filesystem;

using namespace std;

typedef pair<string, int> EDGE_DEGREE;
typedef pair<string, string> EDGE;

struct cmpByDegreeAsc {
    bool operator()(const EDGE_DEGREE& a, const EDGE_DEGREE& b) const {
        return a.second <= b.second;
    }
};

struct cmpByDegreeDesc {
    bool operator()(const EDGE_DEGREE& a, const EDGE_DEGREE& b) const {
        return a.second >= b.second;
    }
};

class Graph 
{ 
    map<string, set<string>> _edges;
    string language;
	int V;
    int E;
	vector<int> *adj; 
    vector<EDGE> inputEdges;

public: 
	Graph(string language, int V); 
    Graph(const Graph& g2);
    Graph();

	void addEdge(int v, int w); 

    void AddEdgeUnderlying(string v, string w); 

	double Closeness(int s); 

    double ClosenessCentrality(); 

    double ClosenessCentralityReduced(int M);

    void TrySwitch(int v1, int v2);

    void CreateAdjOrdered();

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
            for(int i = 0; i<edges; i++){
                inFile >> v1 >> v2;
                if(v1.compare(v2) != 0) {
                    graph.AddEdgeUnderlying(v1, v2);
                    c++;
                }
            }
            inFile.close();
            graph.SetEdgesCount(c);
            graph.CreateAdjOrdered();
            return graph;
        }
        throw new exception;
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
    std::map<string, set<string>>::const_iterator it = g._edges.begin();
    while(it != g._edges.end())
    {
        this->_edges[it->first] = it->second;
        ++it;
    }
    this->adj = new vector<int>[V];
    for (auto i = 0; i<V; i++){
        this->adj[i] = g.adj[i];
    }
}

void Graph::SetEdgesCount(int count){
    this->E = count;
}

void Graph::CreateAdjOrdered(){
    set<EDGE_DEGREE, cmpByDegreeDesc> ordered;
    for(map<string,set<string>>::iterator iter = _edges.begin(); iter != _edges.end(); ++iter){
        ordered.insert(EDGE_DEGREE(iter->first, iter->second.size()));
    }

    map<string, int> m;
    for(set<EDGE_DEGREE>::iterator iter = ordered.begin(); iter != ordered.end(); ++iter){
        m[iter->first] = distance(ordered.begin(), iter);
    }

    // int c = 0;
    // map<string, int> m;
    // for(map<string,set<string>>::iterator iter = _edges.begin(); iter != _edges.end(); ++iter){
    //     m[iter->first] = c++;
    // }

    for(map<string,set<string>>::iterator iter = _edges.begin(); iter != _edges.end(); ++iter){
        for(set<string>::iterator iter2 = iter->second.begin(); iter2 != iter->second.end(); ++iter2){
            this->addEdge(m[iter->first], m[*iter2]);
        }
    }
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

void Graph::AddEdgeUnderlying(string v, string w) 
{
    if(_edges.find(v) == _edges.end()){
        _edges[v] = set<string>(); 
    } 
    if(_edges.find(w) == _edges.end()){
        _edges[w] = set<string>(); 
    } 
	_edges[v].insert(w);
    _edges[w].insert(v);
    inputEdges.push_back(EDGE(v,w));
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

double Graph::Closeness(int s) 
{ 
    int *d = new int[V];
    queue<int> q; 
  	vector<bool> visited;
    visited.assign(V, false); 

    q.push(s);  
    d[s] = 0; 
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
                d[*i] = d[u] + 1; 
                q.push(*i);  
            } 
        } 
    } 
    double closeness = 0.0;
    for(int i = 0; i<V; i++){
        if(d[i] > 0) closeness += 1/(1.0*d[i]);
    }
    return closeness/(1.0*(V-1));
} 

double Graph::ClosenessCentrality(){
    double closeness = 0.0;
    for(int i = 0; i<V; closeness += Closeness(i), i++);
    return closeness/(1.0*V);
} 

double Graph::ClosenessCentralityReduced(int M){
    double closeness = 0.0;
    for(int i = 0; i<M; i++){
        int idx = ((double) rand() / (RAND_MAX)) * V;
        closeness += Closeness(idx);
    }
    // for(int i = 0; i<M; closeness += Closeness(i), i++);
    cout << "Closeness here " << fixed << setprecision(7) << closeness << endl;
    return (closeness/(1.0*V));
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
