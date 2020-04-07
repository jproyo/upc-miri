#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>
#include <fstream>
#include <vector>

using namespace Gecode;
using namespace std;

typedef pair<int, int> DIM;
typedef vector<DIM>    BOX;
int width;
BOX boxes;

class BoxWrapping : public Space {

  protected:
    int colors;
    int vertices;
    bool **graph;
    IntVarArray colVertex;

  public:

    BoxWrapping(int col, bool **g, int vert) : vertices(vert), colors(col), graph(g), colVertex(*this, vertices, 0, colors) {

    // If (i,j) \in E \implies c_i != c_j
    for(int i = 0; i<vertices;i++)
      for(int j=0; j<vertices;j++)
        if(graph[i][j]) rel(*this, colVertex[i] != colVertex[j]);

    branch(*this, colVertex, INT_VAR_NONE(), INT_VAL_MIN());
  }

  BoxWrapping(BoxWrapping& s) : Space(s) {
    colVertex.update(*this, s.colVertex);
    vertices = s.vertices;
    colors = s.colors;
    graph = s.graph;
  }

  virtual Space* copy() {
    return new BoxWrapping(*this);
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


void read_instance() {
  int num;
  cin >> width >> num;
  int n, x, y;
  int c = 1;
  while (num != 0) {
    cin >> n >> x >> y;
    num -= n;
    for(int i = 0; i < n; i++){
      boxes.push_back(DIM(x,y));
    }
    ++c;
  }
}

void show_help(int argc, char* argv[]){
    // Write help message.
  if (argc != 1 and (string(argv[1]) == "-h" or string(argv[1]) == "--help")) {
    cout << "Run the Box Wrapping Constrain Programming Problem" << endl;
    cout << "Usage: " << argv[0] << " < bwp_W_N_k.CP.inp" << endl;
    exit(0);
  }
}

void show_boxes(){
  BOX::iterator it;
  cout << "----BOXES---";
  for (it = boxes.begin(); it < boxes.end(); it++)
    cout << "X: " << (*it).first << " - Y: " << (*it).second << endl;
  cout << endl;
}

int main(int argc, char* argv[]) {
  show_help(argc,argv);
  read_instance();
  show_boxes();
}
//int main(int argc, char* argv[]) {
//  if (argc != 3) return 1;
//  int colors = atoi(argv[1]);
//  int vertices = atoi(argv[2]);
//  bool **graph;
//  graph = new bool *[vertices];
//  for(int i = 0; i <vertices; i++){
//    graph[i] = new bool[vertices];
//  }
//
//  for(int i=0; i<vertices; i++)
//    for(int j=0;j<vertices;j++)
//      (i!=j) ? graph[i][j] = true : graph[i][j] = false;
//
//  BoxWrapping* m = new BoxWrapping(colors, graph, vertices);
//  DFS<BoxWrapping> e(m);
//  m->printGraph();
//  delete m;
//  while (BoxWrapping* s = e.next()) {
//    s->print();
//    delete s;
//  }
//}
