#include <iostream>
#include <vector>

using namespace std;

ostream& operator <<(ostream& out, const vector<int>& t) {
  int n = t.size();
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < n; ++j)
      out << (t[i] == j ? "X" : ".");
    out << endl;
  }
  return out;
}

class NQueens {
  int n;
  vector<int> t, mc, md1, md2;

  int diag2(int i, int j) { return i+j; }
  int diag1(int i, int j) { return i-j + n-1; }

  bool rec(int i) {
    if (i == n) {
      cout << t << endl;
      return true;
    }
    else {
      for (int j = 0; j < n; ++j)
	if (not mc[j]
	    and not md1[diag1(i, j)]
	    and not md2[diag2(i, j)]) {
	  t[i] = j;
	  mc[j] = md1[diag1(i, j)] = md2[diag2(i, j)] = true;
	  if (rec(i+1)) return true;
	  mc[j] = md1[diag1(i, j)] = md2[diag2(i, j)] = false;
	}
      return false;
    }
  }

public:

  NQueens(int n_arg) {
    n = n_arg;
    t = vector<int>(n);
    mc = vector<int>(n, false);
    md1 = md2 = vector<int>(2*n-1, false);
    rec(0);
  }
};

int main(int argc, char* argv[]) {
  if (argc != 2) return 1;
  int n = atoi(argv[1]);
  NQueens r(n);
}
