
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


    double merit_aux(int i) const{
      return boxes.boxArea(i);
    }

    static double merit(const Space& home, IntVar x, int i) {
      return static_cast<const BoxWrapping&>(home).merit_aux(i);
    }

    int value_aux(IntVar x, int i) const {
      int max = 0;
      for(int i = 0; i<boxes.size(); i++){
        if(x_br[i].assigned()){
          if(max <= x_br[i].val()){
            max = x_br[i].val();
          }
        }
      }
      int r = x.min();
      for (IntVarValues k(x); k( ) ; ++k) {
        int j  = k.val();
        if(max+1 < boxes.getWidth()-1 && j > max && j > r){
          r = j;
        }
      }
      return r;
    }

    static int value(const Space& home, IntVar x, int i) {
      return static_cast<const BoxWrapping&>(home).value_aux(x, i);
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

        //It is not rotated
        //Constraint 4 - According to report.pdf
        rel(*this, !r_b[i] >> (x_br[i] == x_tl[i] + (box_width - 1)), PROP_LEVEL);
        //Constraint 5 - According to report.pdf
        rel(*this, !r_b[i] >> (y_br[i] == y_tl[i] + (box_height - 1)), PROP_LEVEL);

        //It is rotated
        //Constraint 6 - According to report.pdf
        rel(*this, r_b[i] >> (x_br[i] == x_tl[i] + (box_height - 1)), PROP_LEVEL);
        //Constraint 7 - According to report.pdf
        rel(*this, r_b[i] >> (y_br[i] == y_tl[i] + (box_width - 1)), PROP_LEVEL);

        for(int j = i+1; j < boxes.size(); j++){

          int box_width_j = boxes.boxWidth(j);
          int box_height_j = boxes.boxHeight(j);

          //Constraint 8 - According to report.pdf
          rel(*this, !r_b[i] >> ((x_tl[i] + box_width - 1) < x_tl[j]) || ((y_tl[i] + box_height - 1) < y_tl[j]), PROP_LEVEL);

          //Constraint 9 - According to report.pdf
          rel(*this, r_b[i] >> ((x_tl[i] + box_height - 1) < x_tl[j]) || ((y_tl[i] + box_width - 1) < y_tl[j]), PROP_LEVEL);

          if(boxes.boxArea(i) < boxes.boxArea(j)){
            //Constraint 10 - According to report.pdf
            rel(*this, x_tl[i] < x_tl[j], PROP_LEVEL);
            //Constraint 11 - According to report.pdf
            rel(*this, y_tl[i] < y_tl[j], PROP_LEVEL);
          }


        }

        //Constraint 12 - According to report.pdf
        rel(*this, element(x_tl, 0) < (boxes.getWidth()/2)-1, PROP_LEVEL);

        //Constraint 13 - According to report.pdf
        rel(*this, element(y_tl,0) < (length/2)-1, PROP_LEVEL);

        //Constraint 14 - According to report.pdf
        rel(*this, length == max(y_br) + 1, PROP_LEVEL);

        //Branch and bound
        //Strategy best success principle.
        //In this case we are going to choose the variable with the smallest domain but in case
        //of tie we are going to choose that one who has a bigger box to try to fill first bigger boxes
        //In the case of the val we are going to try to fill from top left to bottom right. In that sense we are going to look for x_tl more to the left and y_tl more to the top
        //In the case of x_tl val selection we are going to do a custom selection which is the following:
        //Instead of selecting the most left x_tl first try to see if there is width left in the roll. If there is still width in the roll and value of that x_tl is avaible first try to select that, otherwise select min value which is most left.
        branch(*this, x_tl, tiebreak(INT_VAR_SIZE_MIN(), INT_VAR_MERIT_MAX(&merit)), INT_VAL(&value));
        branch(*this, y_tl, tiebreak(INT_VAR_SIZE_MIN(), INT_VAR_MERIT_MAX(&merit)), INT_VAL_MIN());
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
    boxes = s.boxes;
  }

  virtual Space* copy() {
    return new BoxWrapping(*this);
  }

  virtual void constrain(const Space& boxWrap) {
    const BoxWrapping& oldBox = static_cast<const BoxWrapping&>(boxWrap);
    rel(*this, length < oldBox.length);
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
