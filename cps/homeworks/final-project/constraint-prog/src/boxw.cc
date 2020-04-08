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

class BoxWrapping : public Space {

  private:
    int width;
    BOX boxes;

  protected:
    IntVar length;
    IntVarArray x_tl;
    IntVarArray y_tl;
    IntVarArray x_br;
    IntVarArray y_br;
    BoolVarArray r_b;

  public:

    BoxWrapping(int w, int maxLength, const BOX& b) :
      boxes(b),
      width(w),
      length(*this, 0, maxLength),
      x_tl(*this, boxes.size(), 0, w-1),
      y_tl(*this, boxes.size(), 0, maxLength),
      x_br(*this, boxes.size(), 0, w-1),
      y_br(*this, boxes.size(), 0, maxLength),
      r_b(*this, boxes.size(), 0, 1){


      for(int i = 0; i<boxes.size(); i++){
        int box_width = boxes[i].first;
        int box_height = boxes[i].second;
        //Constraint 1 - According to report.pdf
        rel(*this, x_tl[i] <= x_br[i]);
        //Constraint 2 - According to report.pdf
        rel(*this, y_tl[i] <= y_br[i]);
        //Constraint 3 - According to report.pdf
        rel(*this, x_br[i] <= width);

        //It is not rotated
        //Constraint 4 - According to report.pdf
        rel(*this, !r_b[i] >> (x_br[i] == x_tl[i] + (box_width - 1)));
        //Constraint 5 - According to report.pdf
        rel(*this, !r_b[i] >> (y_br[i] == y_tl[i] + (box_height - 1)));

        //It is rotated
        //Constraint 6 - According to report.pdf
        rel(*this, r_b[i] >> (x_br[i] == x_tl[i] + (box_height - 1)));
        //Constraint 7 - According to report.pdf
        rel(*this, r_b[i] >> (y_br[i] == y_tl[i] + (box_width - 1)));

        //Constraint 8 - According to report.pdf
        for(int j = i+1; j < boxes.size(); j++){
          rel(*this, ((x_tl[i] + box_width - 1) < x_tl[j]) || ((y_tl[i] + box_height - 1) < y_tl[j]));
        }

        //Constraint 9 and 10 are given by sort_boxes_bigger_desc

        //Constraint 11 - According to report.pdf
        rel(*this, length == max(y_br) + 1);

        //Branch and bound
        branch(*this, x_tl, INT_VAR_DEGREE_MIN(), INT_VAL_MIN());
        //branch(*this, x_br, INT_VAR_DEGREE_MIN(), INT_VAL_MAX());
        branch(*this, y_tl, INT_VAR_DEGREE_MIN(), INT_VAL_MIN());
        //branch(*this, y_br, INT_VAR_DEGREE_MIN(), INT_VAL_MAX());
        branch(*this, r_b, BOOL_VAR_NONE(), BOOL_VAL_MIN());

      }


  }

  BoxWrapping(BoxWrapping& s) : Space(s) {
    x_tl.update(*this, s.x_tl);
    y_tl.update(*this, s.y_tl);
    x_br.update(*this, s.x_br);
    y_br.update(*this, s.y_br);
    r_b.update(*this, s.r_b);
    length.update(*this, s.length);
    width = s.width;
    boxes = s.boxes;
  }

  virtual Space* copy() {
    return new BoxWrapping(*this);
  }

  virtual void constrain(const Space& boxWrap) {
    const BoxWrapping& oldBox = static_cast<const BoxWrapping&>(boxWrap);
    rel(*this,length < oldBox.length);
  }

  void print(){
    cout << "----DEBUG PURPOSE ONLY-----" << endl;
    cout << "Length: " << length.val() << endl;
    for(int i = 0; i < boxes.size(); i++){
      cout << "BOX: " << i << " x_tl:" << x_tl[i].val() << " - y_tl:" << y_tl[i].val() <<  " - x_br:" << x_br[i].val() << " - y_br:" << y_br[i].val() << " - rotated:" << r_b[i].val() << endl;
    }
    cout << endl;
  }

};


pair<BOX,int> read_instance() {
  BOX boxes;
  int width;
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
  return make_pair(boxes, width);
}

void show_help(int argc, char* argv[]){
    // Write help message.
  if (argc != 1 and (string(argv[1]) == "-h" or string(argv[1]) == "--help")) {
    cout << "Run the Box Wrapping Constrain Programming Problem" << endl;
    cout << "Usage: " << argv[0] << " < bwp_W_N_k.CP.inp" << endl;
    exit(0);
  }
}

void show_boxes(BOX& boxes){
  BOX::iterator it;
  cout << "----BOXES---" << endl;
  for (it = boxes.begin(); it < boxes.end(); it++)
    cout << "X: " << it->first << " - Y: " << it->second << endl;
  cout << endl;
}


int calculateMaxLength(BOX& boxes){
  BOX::iterator it;
  int maxLength = 0;
  for (it = boxes.begin(); it < boxes.end(); it++)
    maxLength += max(it->first, it->second);
  return maxLength;
}

bool sort_boxes_bigger_desc(DIM b1, DIM b2){
  return (b1.first * b1.second) > (b2.first * b2.second);
}

int main(int argc, char* argv[]) {

  show_help(argc,argv);

  pair<BOX, int> instance = read_instance();

  BOX boxes = instance.first;

  int width = instance.second;

  int maxLength = calculateMaxLength(boxes);

  // Sort boxes Bigger first
  sort(boxes.begin(), boxes.end(), sort_boxes_bigger_desc);

  show_boxes(boxes);

  cout << "Max length: " << maxLength << " - Max width: " << width << endl;

  BoxWrapping* m = new BoxWrapping(width, maxLength, boxes);
  BAB<BoxWrapping> e(m);
  delete m;
  BoxWrapping* newSol;
  while(BoxWrapping* sol = e.next()) {
    if(sol) newSol = sol;
  }
  if(newSol) {
    newSol->print();
  }
  delete newSol;
}
