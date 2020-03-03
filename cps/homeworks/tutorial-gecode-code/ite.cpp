#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace Gecode;
using namespace std;

class A : public Space {
protected:
  IntVar x;
public:
  A() : x(*this, 0, 9) {
    rel(*this, x != 3);
    rel(*this, x != 6);

    cout << endl << "****************" << endl;
    for (IntVarValues i(x); i(); ++i)
    cout << i.val() << ' ';
    cout << endl << "****************" << endl;
    for (IntVarRanges i(x); i(); ++i)
    cout << i.min() << ".." << i.max() << ' ';
    cout << endl << "****************" << endl;

    branch(*this, x, INT_VAL_MIN());
  }
  A(A& s) : Space(s) {
    x.update(*this, s.x);
  }
  virtual Space* copy() {
    return new A(*this);
  }
  void print() const {
    cout << x << endl;
  }
};

int main() {
  A* m = new A;
  DFS<A> e(m);
  delete m;
  while (A* s = e.next()) {
    s->print(); delete s;
  }
}
