#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace Gecode;
using namespace std;

class Queens : public Space {

  protected:
    int n;
    IntVarArray queens;

  public:

    Queens(int num): n(num), queens(*this, num, 0, num-1) {

      // N queens in the board
      for(int i = 0; i < n; i++){
        for(int j= i+1; j < n; j++){
          //One queen per row and per col
          rel(*this, queens[i] != queens[j]);
          //Diagonals
          rel(*this, queens[i] + i != queens[j] + j);
          rel(*this, queens[i] - i != queens[j] - j);
        }
      }

      branch(*this, queens, INT_VAR_NONE(), INT_VAL_MIN());
    }

    Queens(Queens& s) : Space(s) {
      queens.update(*this, s.queens);
      n = s.n;
    }

    virtual Space* copy() {
      return new Queens(*this);
    }

    void print() const {
      for(int i = 0; i < n; i++){
        for(int j = 0; j < n; j++)
          cout << (queens[i].val() == j ? "X" : ".");
        cout << endl;
      }
    }
};

int main(int argc, char* argv[]) {
  try {
    if (argc != 2) return 1;
    int n = atoi(argv[1]);
    Queens* m = new Queens(n);
    DFS<Queens> e(m);
    delete m;
    if (Queens* s = e.next()) {
      s->print(); delete s;
    }
  }catch(Exception e){
    cerr << "Gecode exception: " << e.what() << endl;
    return 1;
  }
  return 0;
}
