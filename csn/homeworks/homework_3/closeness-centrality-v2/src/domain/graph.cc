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

namespace fs = std::__fs::filesystem;

using namespace std;

typedef pair<string, int> EDGE;

struct cmpByDegreeAsc {
    bool operator()(const EDGE& a, const EDGE& b) const {
        return a.second <= b.second;
    }
};

class Graph 
{ 
    map<string, set<string>> _edges;
    string language;
	int V;
    int E;
	list<int> *adj; 

public: 
	Graph(string language, int V); 
    Graph(const Graph& g2);

	void addEdge(int v, int w); 

    void AddEdgeUnderlying(string v, string w); 

	double Closeness(int s); 

    double ClosenessCentrality(); 

    double ClosenessCentralityReduced();

    void CreateAdjOrdered();

    void PrintAdjList();

    void PrintTable1Report();

    string GetLanguage();

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
                    graph.AddEdgeUnderlying(v2, v1);
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

Graph::Graph(string language, int V) 
{ 
    this->language = language;
	this->V = V; 
	adj = new list<int>[V]; 
} 

Graph::Graph(const Graph &g): language(g.language), V(g.V), E(g.E), _edges(g._edges), adj(g.adj) {}

void Graph::SetEdgesCount(int count){
    this->E = count;
}

void Graph::CreateAdjOrdered(){
    set<EDGE, cmpByDegreeAsc> ordered;
    for(map<string,set<string>>::iterator iter = _edges.begin(); iter != _edges.end(); ++iter){
        ordered.insert(EDGE(iter->first, iter->second.size()));
    }
    int c = 0;
    map<string, int> m;
    for(set<EDGE>::iterator iter = ordered.begin(); iter != ordered.end(); ++iter){
        m[iter->first] = distance(ordered.begin(), iter);
    }
    for(map<string,set<string>>::iterator iter = _edges.begin(); iter != _edges.end(); ++iter){
        for(set<string>::iterator iter2 = iter->second.begin(); iter2 != iter->second.end(); ++iter2){
            this->addEdge(m[iter->first], m[*iter2]);
        }
    }
}

void Graph::PrintAdjList(){
    for(int i = 0;i<V;i++){
        cout << "Vertex: " << i << endl;
        for(list<int>::iterator iter = adj[i].begin(); iter != adj[i].end(); ++iter)
            cout << "Adj List: "<< *iter << "|";
        cout << endl;
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
    double closeness = 0.0;
	bool *visited = new bool[V]; 
	for(int i = 0; i < V; visited[i] = false, i++);

	list<int> queue; 

	visited[s] = true; 
	queue.push_back(s); 

	list<int>::iterator i; 

    int distance = 1;
	while(!queue.empty()) 
	{ 
		s = queue.front(); 
		queue.pop_front(); 

		for (i = adj[s].begin(); i != adj[s].end(); ++i) 
		{ 
			if (!visited[*i]) { 
                closeness = closeness + (1/(1.0*distance));
				visited[*i] = true; 
				queue.push_back(*i); 
			} 
		} 
        distance++;
	} 
    return closeness/(1.0*(V-1));
} 

double Graph::ClosenessCentrality(){
    double closeness = 0.0;
    for(int i = 0; i<V; closeness += Closeness(i), i++);
    return closeness/(1.0*V);
} 

double Graph::ClosenessCentralityReduced(){
    return 0.0;
}

int main(int argc, char* argv[]) {

    std::string path = argv[1];
    Graph g = Graph::fromStdIn(path);
    g.PrintTable1Report();
    chrono::steady_clock::time_point begin = chrono::steady_clock::now();
    double closeness = g.ClosenessCentrality();
    chrono::steady_clock::time_point end = chrono::steady_clock::now();
    cout << "Closeness for Language " << g.GetLanguage() << " " << fixed << std::setprecision(7) << closeness << endl;
    cout << "Elapsed time: " << fixed << std::setprecision(2) << (chrono::duration_cast<std::chrono::microseconds>(end - begin).count()) << " microsec " << endl;

	return 0; 
} 
