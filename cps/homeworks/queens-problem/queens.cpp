/*
 *  Authors:
 *    Christian Schulte <schulte@gecode.org>
 *
 *  Copyright:
 *    Christian Schulte, 2008-2013
 *
 *  Permission is hereby granted, free of charge, to any person obtaining
 *  a copy of this software, to deal in the software without restriction,
 *  including without limitation the rights to use, copy, modify, merge,
 *  publish, distribute, sublicense, and/or sell copies of the software,
 *  and to permit persons to whom the software is furnished to do so, subject
 *  to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be
 *  included in all copies or substantial portions of the software.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 *  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

using namespace Gecode;

class Queens : public Space {

  protected:
    int n;
    BoolVarArray b;

  public:

    SendMoreMoney() : l(*this, 8, 0, 9) { // initialize array and start constructor
    IntVar s(l[0]), e(l[1]), n(l[2]), d(l[3]),
           m(l[4]), o(l[5]), r(l[6]), y(l[7]);
    rel(*this, s != 0); // rel ~ Constraint relacional
    rel(*this, m != 0);
    distinct(*this, l); // same as all_diferent
    rel(*this,             1000*s + 100*e + 10*n + d
                         + 1000*m + 100*o + 10*r + e
              == 10000*m + 1000*o + 100*n + 10*e + y);
    branch(*this, l, INT_VAR_SIZE_MIN(), INT_VAL_MIN()); // branch = Search. INT_VAR_SIZE_MIN is because in the backtracking it is choosing the variable with min size in their domain. INT_VAL_MIN choose the minimum value of the domain.
  }
  SendMoreMoney(SendMoreMoney& s) : Space(s) {
    l.update(*this, s.l);
  }
  virtual Space* copy() {
    return new SendMoreMoney(*this);
  }
  void print() const {
    std::cout << l << std::endl;
  }
};

int main() {
  SendMoreMoney* m = new SendMoreMoney;
  DFS<SendMoreMoney> e(m);
  delete m;
  while (SendMoreMoney* s = e.next()) {
    s->print(); delete s;
  }
}
