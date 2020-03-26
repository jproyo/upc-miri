#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <cassert>

using namespace std;

int main(int argc, char** argv) {

  // Write help message.
  if (argc == 1 or
      (argc == 2 and (string(argv[1]) == "-h" or string(argv[1]) == "--help"))) {
    cout << "Checks solutions to the queens problem" << endl;
    cout << "Usage: " << argv[0] << " <file with queens solution>" << endl;
    exit(0);
  }

  // Open input stream.
  assert(argc == 2);
  ifstream in(argv[1]);
  assert(in.good());

  // Read board.
  string line;
  getline(in, line);
  int n = line.size();
  vector< vector<bool> > board(n, vector<bool>(n, false));
  int row = 0;
  int all = 0;
  do {
    for (int j = 0; j < n; ++j)
      if (line[j] == 'X') {
	board[row][j] = true;
	++all;
      }
      else assert(line[j] == '.');
    ++row;
  } while (getline(in, line));

  // Check solution is correct.
  if (all != n) {
    cout << "Error! Number of queens different from " << n << ", "
	 << "found " << all << " queens" << endl;
    exit(1);
  }

  for (int i = 0; i < n; ++i) {
    int cnt = 0;
    for (int j = 0; j < n; ++j)
      if (board[i][j]) ++cnt;
    if (cnt > 1) {
      cout << "Error! There are at least two attacking queens at row " << i+1 << endl;
      exit(1);
    }
  }

  for (int j = 0; j < n; ++j) {
    int cnt = 0;
    for (int i = 0; i < n; ++i)
      if (board[i][j]) ++cnt;
    if (cnt > 1) {
      cout << "Error! There are at least two attacking queens at column " << j+1 << endl;
      exit(1);
    }
  }

  for (int k = 0; k <= 2*n-2; ++k) {
    // diagonal: i + j = k (0 <= i,j < n)
    int cnt = 0;
    // j = k-i >= 0   --> i <= k
    // j = k-i <= n-1 --> i >= k-n+1
    for (int i = max(0, k-n+1); i <= min(n-1, k); ++i) {
      int j = k-i;
      if (board[i][j]) ++cnt;
    }
    if (cnt > 1) {
      cout << "Error! There are at least two attacking queens at diagonal r+c =" << k+2 << endl;
      exit(1);
    }
  }

  for (int k = 1-n; k <= n-1; ++k) {
    // diagonal: i - j = k (0 <= i,j < n)
    int cnt = 0;
    // j = k+i >= 0   --> i >= -k
    // j = k+i <= n-1 --> i <= -k+n-1
    for (int i = max(0, -k); i <= min(n-1, -k+n-1); ++i) {
      int j = k+i;
      if (board[i][j]) ++cnt;
    }
    if (cnt > 1) {
      cout << "Error! There are at least two attacking queens at diagonal r-c =" << k << endl;
      exit(1);
    }
  }
  cout << "OK!" << endl;
}
