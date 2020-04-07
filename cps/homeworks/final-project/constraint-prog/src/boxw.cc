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
    int width;
    int length;
    IntVarArray x_tl;
    IntVarArray y_tl;
    IntVarArray x_br;
    IntVarArray y_br;
    BoolVarArray r_b;

  public:

    BoxWrapping(int w, int maxLength, BOX boxes) :
      width(w),
      length(maxLength),
      x_tl(*this, w, 1, boxes.size()),
      y_tl(*this, maxLength, 1, boxes.size()),
      x_br(*this, w, 1, boxes.size()),
      y_br(*this, maxLength, 1, boxes.size()),
      r_b(*this, boxes.size(), 0, 1){

  }

  BoxWrapping(BoxWrapping& s) : Space(s) {
    x_tl.update(*this, s.x_tl);
    y_tl.update(*this, s.y_tl);
    x_br.update(*this, s.x_br);
    y_br.update(*this, s.y_br);
    r_b.update(*this, s.r_b);
    width = s.width;
    length = s.length;
  }

  virtual Space* copy() {
    return new BoxWrapping(*this);
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
    cout << "X: " << it->first << " - Y: " << it->second << endl;
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
