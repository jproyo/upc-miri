#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace Gecode;

class GraphColor : public Space {

  protected:
    int colors;
    int vertices;
    bool **graph;
    IntVarArray colVertex;

  public:

    GraphColor(int col, bool **g, int vert) : vertices(vert), colors(col), graph(g), colVertex(*this, vertices, 0, colors) {

    // If (i,j) \in E \implies c_i != c_j
    for(int i = 0; i<vertices;i++)
      for(int j=0; j<vertices;j++)
        if(graph[i][j]) rel(*this, colVertex[i] != colVertex[j]);

    branch(*this, colVertex, INT_VAR_NONE(), INT_VAL_MIN());
  }

  GraphColor(GraphColor& s) : Space(s) {
    colVertex.update(*this, s.colVertex);
    vertices = s.vertices;
    colors = s.colors;
    graph = s.graph;
  }

  virtual Space* copy() {
    return new GraphColor(*this);
  }
  void print() const {
    std::cout << colVertex << std::endl;
  }

  void printGraph() const {
    for(int i = 0; i<vertices;i++)
      for(int j=0; j<vertices;j++)
        if(graph[i][j]) std::cout << i << " -> " << j << std::endl;
  }
};

int main(int argc, char* argv[]) {
  if (argc != 3) return 1;
  int colors = atoi(argv[1]);
  int vertices = atoi(argv[2]);
  bool **graph;
  graph = new bool *[vertices];
  for(int i = 0; i <vertices; i++){
    graph[i] = new bool[vertices];
  }

  for(int i=0; i<vertices; i++)
    for(int j=0;j<vertices;j++)
      (i!=j) ? graph[i][j] = true : graph[i][j] = false;

  GraphColor* m = new GraphColor(colors, graph, vertices);
  DFS<GraphColor> e(m);
  m->printGraph();
  delete m;
  while (GraphColor* s = e.next()) {
    s->print();
    delete s;
  }
}
