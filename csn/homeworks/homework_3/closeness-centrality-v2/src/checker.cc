#include <iostream>
#include <fstream>
#include <sstream>
#include <cassert>
#include <cstdlib>
#include <map>
#include <vector>

using namespace std;

typedef pair<int, int> P;
typedef map<P, int>    M;
typedef pair<P, P>     PP;
typedef pair<P, int>   PI;
typedef vector<PP>     VPP;
typedef vector<PI>     VPI;
typedef vector<VPI>    VVPI;

const PI UNDEF(P(0, 0), 0);

ifstream in_ins, in_sol;
int  width, length;
M    order, order0, color;
VPP  sol;
VVPI board;
bool ok;

ostream& operator << (ostream& out, const P& p) {
  out << "(" << p.first << ", " << p.second << ")";
  return out;
}


bool within_bounds(P point) {
  return
    0 <= point.first  and point.first  < width and
    0 <= point.second and point.second < length;
}

void check_solution() {

  ok = true;
  order0 = order;
  board = VVPI(length, VPI(width, UNDEF));

  for (auto p : sol) {
    P tl = p.first;
    if (not within_bounds(tl)) {
      ok = false;
      cout << "Error: position " << tl << " is out of bounds" << endl;
      exit(1);
    }
    P br = p.second;
    if (not within_bounds(br)) {
      ok = false;
      cout << "Error: position " << br << " is out of bounds" << endl;
      exit(1);
    }
    int dx = br.first  - tl.first + 1;
    int dy = br.second - tl.second + 1;
    P rec = (dx <= dy ? P(dx, dy) : P(dy, dx));
    if (order.count(rec) == 0) {
      ok = false;
      cout << "Error: rectangle of dimensions " << rec.first << "x" << rec.second << " defined by positions top-left corner " << tl << " and bottom-right corner " << br << " does not match any in input data" << endl;
      if (dx <= 0 or dy <= 0)
	exit(1);
    }
    --order[rec];
    if (order[rec] < 0) {
      ok = false;
      cout << "Error: too many rectangles of dimensions " << rec.first << "x" << rec.second << endl;
    }
    for (int i = tl.second; i <= br.second; ++i)
      for (int j = tl.first; j <= br.first; ++j) {
	if (board[i][j] != UNDEF) {
	  ok = false;
	  cout << "Error:"
	       << " rectangle defined by top-left corner " << tl << " and bottom-right corner " << br
	       << " overlaps"
	       << " rectangle of dimensions " << board[i][j].first.first << "x" << board[i][j].first.second
	       << " at position " << P(j, i) << endl;
	}
	if (order[rec] < 0) board[i][j] = PI(rec, 0);
	else                board[i][j] = PI(rec, order[rec]);
	
      }
  }

  for (auto p : order)
    if (p.second > 0) {
      ok = false;
      cout << "Error: at least one rectangle of dimensions " << p.first.first << "x" << p.first.second << " is missing" << endl;
    }

  if (ok) cout << "OK" << endl;
  cout << endl;
}

string int2string(int x) {
  ostringstream out;
  out << x;
  return out.str();
}

const int MAX_COLORS = 256;

string set_color(int c) {
  assert(0 <= c and c < MAX_COLORS);
  return string("\033[m\033[48;5;" + int2string(c) + "m").c_str();
}

string unset_color() {
  return "\033[0m";
}

void display_solution() {
  if (order.size() > MAX_COLORS) {
    cout << "Cannot display solution: too many colors" << endl;
    exit(1);
  }
  for (int i = 0; i < length; ++i)
    for (int j = 0; j < width; ++j)
      if (board[i][j].second > 'z' - 'a' + 1) {
	cout << "Cannot display solution: too many rectangles of the same dimension" << endl;
	exit(1);
      }
  for (int i = 0; i < length; ++i) {
    cout << "     ";
    for (int j = 0; j < width; ++j) {
      if (board[i][j] == UNDEF) cout << ' ';
      else {
	int c = color[board[i][j].first];
	cout << set_color(c) << char('a' + board[i][j].second) << unset_color();
      }
    }
    cout << endl;
  }
  cout << endl << endl;

  cout << "Input data:" << endl << endl;
  for (auto p: color) {
    cout << set_color(p.second) << p.first.first << "x" << p.first.second << unset_color() 
	 << ":" << order0[p.first] << endl;
  }
}


void read_instance() {
  int num;
  cin >> width >> num;
  int n, x, y;
  int c = 1;
  while (num != 0) {
    cin >> n >> x >> y;
    num -= n;
    order[P(x, y)] = n;
    color[P(x, y)] = c;
    ++c;
  }

  // cout << "W: " << width << endl;
  // for (auto i = order.begin(); i != order.end(); ++i) {
  //   cout << i->first << " => " << i->second << endl;
  // }
}


void read_solution() {
  cin >> length;
  int xtl, ytl, xbr, ybr;
  while (cin >> xtl >> ytl >> xbr >> ybr)
    sol.push_back(PP(P(xtl, ytl), P(xbr, ybr)));

  // cout << "L: " << length << endl;
  // for (auto p : sol)
  //   cout << p.first << " - " << p.second << endl;
}


int main(int argc, char** argv) {

  // Write help message.
  if (argc != 1 and (string(argv[1]) == "-h" or string(argv[1]) == "--help")) {
    cout << "Makes a sanity check of a solution to the Box Wrapping Problem" << endl;
    cout << "Usage: " << argv[0] << " < bwp_W_N_k.CP.out" << endl;
    exit(0);
  }

  read_instance();
  read_solution();

  check_solution();
  display_solution();
}
