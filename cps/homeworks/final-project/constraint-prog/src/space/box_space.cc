
/**
 *
 * This class contains the Space subclass with constraint
 *
 * @author: Juan Pablo Royo Sales
 * @date: April 6th, 2020
 * @subject: CPS
 * @institution: Universitat Politècnica Catalunya
 * @title: Final Project
 *
 */

#include <cstdlib>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>

#ifndef BOX_WRAPPER
#define BOX_WRAPPER
#include "../domain/box_wrapper.cc"
#endif

using namespace Gecode;
using namespace std;

class BoxWrapping : public Space {

  private:
    Boxes boxes;
    static IntPropLevel PROP_LEVEL;

  protected:
    IntVar length;
    IntVarArray x_tl;
    IntVarArray y_tl;
    IntVarArray x_br;
    IntVarArray y_br;
    BoolVarArray r_b;

  public:

    int value_aux(IntVar x, int i) const {
      if(i == 0) return x.min();

      int b_w = boxes.boxWidth(i-1);
      int b_h = boxes.boxHeight(i-1);
      int maximum = max(b_w, b_h);
      if (maximum > boxes.getWidth()){
        maximum = min(b_w, b_h);
      }

      int x_tl_before = x_tl[i-1].val() + maximum;

      int val = x.min();
      for (IntVarValues v(x); v( ) ; ++v) {
        if(v.val() >= x_tl_before) {
           val = v.val();
           break;
        }
      }
      return val;
    }


    static int value(const Space& home, IntVar x, int i) {
      return static_cast<const BoxWrapping&>(home).value_aux(x, i);
    }

    static void commit(Space& home, unsigned int a,
  		IntVar x, int i, int n) {
      if (a == 0U) rel(home, x, IRT_EQ, n);
      else         rel(home, x, IRT_NQ, n);
    }

    BoxWrapping(const Boxes& b) :
      boxes(b),
      length(*this, 0, b.getMaxLength()),
      x_tl(*this, boxes.size(), 0, b.getWidth()-1),
      y_tl(*this, boxes.size(), 0, b.getMaxLength()),
      x_br(*this, boxes.size(), 0, b.getWidth()-1),
      y_br(*this, boxes.size(), 0, b.getMaxLength()),
      r_b(*this, boxes.size(), 0, 1){

      for(int i = 0; i<boxes.size(); i++){
        int box_width = boxes.boxWidth(i);
        int box_height = boxes.boxHeight(i);

        //Constraint 1 - According to report.pdf
        rel(*this, x_tl[i] <= x_br[i], PROP_LEVEL);
        //Constraint 2 - According to report.pdf
        rel(*this, y_tl[i] <= y_br[i], PROP_LEVEL);
        //Constraint 3 - According to report.pdf
        rel(*this, x_br[i] <= boxes.getWidth(), PROP_LEVEL);

        //It is rotated
        //Constraint 6 - According to report.pdf
        rel(*this, r_b[i] >> (x_br[i] == x_tl[i] + (box_height - 1)), PROP_LEVEL);
        //Constraint 7 - According to report.pdf
        rel(*this, r_b[i] >> (y_br[i] == y_tl[i] + (box_width - 1)), PROP_LEVEL);

        //It is not rotated
        //Constraint 4 - According to report.pdf
        rel(*this, !r_b[i] >> (x_br[i] == x_tl[i] + (box_width - 1)), PROP_LEVEL);
        //Constraint 5 - According to report.pdf
        rel(*this, !r_b[i] >> (y_br[i] == y_tl[i] + (box_height - 1)), PROP_LEVEL);

        for(int j = i+1; j < boxes.size(); j++){

          //Constraint 8 according to report.pdf
          rel(*this, x_br[i] + 1 <= x_tl[j] ||
             y_br[i] + 1 <= y_tl[j] ||
             x_br[j] + 1 <= x_tl[i] ||
             y_br[j] + 1 <= y_tl[i]
             , PROP_LEVEL
           );

        }

        //Constraint 12 - According to report.pdf
        rel(*this, element(x_tl,0) == 0, PROP_LEVEL);

        //Constraint 13 - According to report.pdf
        rel(*this, element(y_tl,0) == 0, PROP_LEVEL);

        //Constraint 14 - According to report.pdf
        rel(*this, length == max(y_br)+1, PROP_LEVEL);

        //Branch and bound
        branch(*this, x_tl, INT_VAR_NONE(), INT_VAL(&value, &commit));
        branch(*this, y_tl, INT_VAR_NONE(), INT_VAL_MIN());
        branch(*this, r_b, BOOL_VAR_NONE(), BOOL_VAL_MAX());

      }


  }

  BoxWrapping(BoxWrapping& s) : Space(s) {
    x_tl.update(*this, s.x_tl);
    y_tl.update(*this, s.y_tl);
    x_br.update(*this, s.x_br);
    y_br.update(*this, s.y_br);
    r_b.update(*this, s.r_b);
    length.update(*this, s.length);
    boxes = s.boxes;
  }

  virtual Space* copy() {
    return new BoxWrapping(*this);
  }

  virtual void constrain(const Space& boxWrap) {
    const BoxWrapping& oldBox = static_cast<const BoxWrapping&>(boxWrap);
    rel(*this, length < oldBox.length.min());
  }

  void printDebug(){
    cerr << "----DEBUG PURPOSE ONLY-----" << endl;
    cerr << "Length: " << length.val() << endl;
    for(int i = 0; i < boxes.size(); i++){
      cerr << "BOX: " << i << " x_tl:" << x_tl[i].val() << " - y_tl:" << y_tl[i].val() <<  " - x_br:" << x_br[i].val() << " - y_br:" << y_br[i].val() << " - rotated:" << r_b[i].val() << endl;
    }
    cerr << endl;
  }

  void show() {
    cout << length.val() << endl;
    for(int i = 0; i < boxes.size(); i++){
      cout << x_tl[i].val() << " " << y_tl[i].val() <<  "   " << x_br[i].val() << " " << y_br[i].val() << endl;
    }
   }



};

// Domain Propagation
IntPropLevel BoxWrapping::PROP_LEVEL = IPL_DOM;
