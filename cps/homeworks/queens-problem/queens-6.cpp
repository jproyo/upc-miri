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

      distinct(*this, queens, IPL_BND);
      distinct(*this, IntArgs::create(n, 0,  1), queens, IPL_BND);
      distinct(*this, IntArgs::create(n, 0, -1), queens, IPL_BND);

      if (n % 2 == 0) rel(*this, queens[0] <  n/2);
      if (n % 2 == 1) rel(*this, queens[0] <= n/2);

      rel(*this, queens[0] <= queens[n-1]);

      branch(*this, queens, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
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
