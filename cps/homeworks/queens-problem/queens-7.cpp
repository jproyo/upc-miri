#include <cassert>
#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace std;
using namespace Gecode;


class Queens : public Space {

private:
  int n;  
  IntVarArray c, r;

public:

  double merit_aux(IntVar x, int i) const {
    return abs(i - n/2.);
  }

  static double merit(const Space& home, IntVar x, int i) {
    return static_cast<const Queens&>(home).merit_aux(x, i);
  }

  int value_aux(IntVar x, int i) const {
    int minj = -1;
    int mins = n+1;
    for (IntVarValues k(x); k( ) ; ++k) {
      int j  = k.val();
      if (mins > int(r[j].size())) {
    	minj = j;
    	mins = r[j].size();
      }
      else if (mins == int(r[j].size()) and abs(j - n/2.) < abs(minj - n/2.)) {
    	minj = j;
      }
    }
    return minj;
  }

  static int value(const Space& home, IntVar x, int i) {
    return static_cast<const Queens&>(home).value_aux(x,i);
  }

  static void commit(Space& home, unsigned int a,
		IntVar x, int i, int n) {
    if (a == 0) rel(home, x, IRT_EQ, n);
    else        rel(home, x, IRT_NQ, n);
  }

  Queens(int nn) : n(nn), c(*this, n, 0, n-1), r(*this, n, 0, n-1) {

    // distinct(*this, c, ICL_DOM); // Old Gecode
    distinct(*this, c, IPL_DOM);    // New Gecode
    distinct(*this, IntArgs::create(n, 0,  1), c, IPL_DOM);
    distinct(*this, IntArgs::create(n, 0, -1), c, IPL_DOM);

    distinct(*this, r, IPL_DOM);
    distinct(*this, IntArgs::create(n, 0,  1), r, IPL_DOM);
    distinct(*this, IntArgs::create(n, 0, -1), r, IPL_DOM);

    channel(*this, c, r);

    // By symmetry over the middle vertical line
    if (n % 2 == 0) rel(*this, c[0] <  n/2);
    if (n % 2 == 1) rel(*this, c[0] <= n/2);

    // By symmetry over the middle horizontal line
    rel(*this, c[0] <= c[n-1]);

    branch(*this, c, tiebreak(INT_VAR_SIZE_MIN(), INT_VAR_MERIT_MIN(&merit)), INT_VAL(&value, &commit));
  }

  Queens(Queens& s) : Space(s) {
    c.update(*this, s.c);
    r.update(*this, s.r);
    n = s.n;
  }

  virtual Space* copy() {
    return new Queens(*this);
  }

  void print() const {
    for (int i = 0; i < n; ++i) {
      for (int j = 0; j < n; ++j)
	cout << (c[i].val() == j ? 'X' : '.');
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
      s->print();
      delete s;
    }
  } catch (Exception e) {
    std::cerr << "Gecode exception: " << e.what() << std::endl;
    return 1;
  }
  return 0;
}
