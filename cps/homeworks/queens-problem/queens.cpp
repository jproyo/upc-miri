#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace Gecode;
using namespace std;

class Queens : public Space {

  protected:
    int n;
    BoolVarArray queens;

  public:

    Queens(int num): n(num), queens(*this, num*num, 0, 1) {

      // N queens in the board
      linear(*this, queens, IRT_EQ, n);

      //Each row 1 queen
      for(int i = 0; i < n; i++){
        BoolVarArgs row(n);
        for(int j= 0; j < n; j++){
          row[j] = queens[i*n+j];
        }
        linear(*this, row, IRT_EQ, 1);
      }

      //Each column 1 queen
      for(int j = 0; j < n; j++){
        BoolVarArgs column(n);
        for(int i= 0; i < n; i++){
          column[i] = queens[i*n+j];
        }
        linear(*this, column, IRT_EQ, 1);
      }

      //Each Diagonal 1 queen
      for (int k = 1-n; k <= n-1; ++k) {
        int l = max(0, k);
        int u = min(n-1, k+n-1);
        BoolVarArgs diag(u-l+1);
        for (int i = l; i <= u; ++i) {
	        int j = i - k;
	        diag[i-l] = queens[i*n+j];
        }
        linear(*this, diag, IRT_LQ, 1);
      }


      for (int k = 0; k <= 2*n-2; ++k) {
        int l = max(0, k-n+1);
        int u = min(n-1, k);
        BoolVarArgs diag(u-l+1);
        for (int i = l; i <= u; ++i) {
	        int j = k - i;
	        diag[i-l] = queens[i*n+j];
        }
        linear(*this, diag, IRT_LQ, 1);
      }

      branch(*this, queens, BOOL_VAR_NONE(), BOOL_VAL_MIN());
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
        for(int j=0; j < n; j++) cout << (queens[i*n+j].val() ? "X" : ".");
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
