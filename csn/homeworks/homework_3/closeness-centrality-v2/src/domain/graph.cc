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
        return a.second < b.second;
    }
};

class Graph 
{ 
    map<string, set<string>> _edges;
    string language;
	int V;
	list<int> *adj; 

public: 
	Graph(string language, int V); 

	void addEdge(int v, int w); 

    void AddEdgeUnderlying(string v, string w); 

	double Closeness(int s); 

    double ClosenessCentrality(); 

    void CreateAdjOrdered();

    void PrintAdjList();

    string GetLanguage();

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
            
            // map<string, int> m;
            // for(int i = 0; i<=edges; i++){
            //     inFile >> v1 >> v2;
            //     if(v1.compare(v2) != 0) {
            //         graph.AddEdgeUnderlying(v1, v2);
            //         if(m.find(v1) == m.end()) m[v1] = c++;
            //         if(m.find(v2) == m.end()) m[v2] = c++;
            //         graph.addEdge(m[v1], m[v2]);
            //     }
            // }

            for(int i = 0; i<=edges; i++){
                inFile >> v1 >> v2;
                if(v1.compare(v2) != 0) {
                    graph.AddEdgeUnderlying(v1, v2);
                }
            }
            inFile.close();
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
	_edges[v].insert(w);
} 

string Graph::GetLanguage(){
    return this->language;
}

double Graph::Closeness(int s) 
{ 
    float closeness = 0.0;
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
            closeness += 1/(1.0*distance);
			if (!visited[*i]) { 
				visited[*i] = true; 
				queue.push_back(*i); 
			} 
		} 
        distance++;
	} 
    return closeness*(1/(1.0*(V-1)));
} 

double Graph::ClosenessCentrality(){
    double closeness = 0.0;
    for(int i = 0; i<V; closeness += Closeness(i), i++);
    return closeness/(1.0*V);
} 

int main(int argc, char* argv[]) {

    std::string path = argv[1];
    for (const auto & entry : fs::directory_iterator(path)){
        chrono::steady_clock::time_point begin = chrono::steady_clock::now();
        Graph g = Graph::fromStdIn(entry.path());
        chrono::steady_clock::time_point end = chrono::steady_clock::now();
	    cout << "Closeness for Language " << g.GetLanguage() << " " << fixed << std::setprecision(7) << g.ClosenessCentrality() << endl;
        cout << "Elapsed time: " << fixed << std::setprecision(2) << (chrono::duration_cast<std::chrono::microseconds>(end - begin).count())/(1.0*100000) << "s" << endl;
    }

	return 0; 
} 
