/**
 *
 * Header file of class Boxes
 * This class contains all the utilities regarding Box and boxes
 * handler.
 *
 * @author: Juan Pablo Royo Sales
 * @date: April 8th, 2020
 * @subject: CPS
 * @institution: Universitat Politècnica Catalunya
 * @title: Final Project
 *
 */

#include <cstdlib>
#include <vector>
#include <iostream>

using namespace std;

typedef pair<int, int> DIM;
typedef vector<DIM>    BOX;
typedef pair<int,DIM> CONF;
typedef vector<CONF> INITIAL_CONF;

class Boxes {

  private:
    int width;
    int maxLength;
    BOX boxes;
    INITIAL_CONF conf;

    Boxes(int w, const BOX& b, const INITIAL_CONF& c): width(w), boxes(b), conf(c){
      sort(boxes.begin(), boxes.end(), sort_boxes_bigger_desc);
      maxLength = calculateMaxLength();
    };

    int calculateMaxLength(){
      BOX::iterator it;
      int maxLength = 0;
      for (it = boxes.begin(); it < boxes.end(); it++)
        maxLength += max(it->first, it->second);
      return maxLength;
    };

    static bool sort_boxes_bigger_desc(DIM b1, DIM b2){
      return (b1.first * b1.second) > (b2.first * b2.second);
    };


  public:
    Boxes();
    Boxes(const Boxes& b2);
    int getWidth() const;
    int getMaxLength() const;
    int size() const;
    int boxWidth(int index) const;
    int boxHeight(int index) const;
    int boxArea(int index) const;
    int isSquare(int index) const;
    void printDebug();
    void show() const;

  static Boxes& fromStdIn(){
    BOX bs;
    INITIAL_CONF cf;
    int w;
    int num;
    cin >> w >> num;
    int n, x, y;
    int c = 1;
    while (num != 0) {
      cin >> n >> x >> y;
      DIM dim(x,y);
      cf.push_back(CONF(n,dim));
      num -= n;
      for(int i = 0; i < n; i++){
        bs.push_back(dim);
      }
      ++c;
    }

    static Boxes b(w,bs,cf);
    return b;
  }
};

Boxes::Boxes(){}

Boxes::Boxes(const Boxes& b2): width(b2.width), maxLength(b2.maxLength), boxes(b2.boxes), conf(b2.conf){}

int Boxes::getWidth() const{
  return this->width;
}

int Boxes::getMaxLength() const{
  return this->maxLength;
}

int Boxes::size() const{
  return this->boxes.size();
}

int Boxes::boxWidth(int index) const{
  return this->boxes[index].first;
}


int Boxes::boxHeight(int index) const{
  return this->boxes[index].second;
}

int Boxes::boxArea(int index) const{
  return this->boxWidth(index)*this->boxHeight(index);
}

int Boxes::isSquare(int index) const{
  return this->boxWidth(index) == this->boxHeight(index);
}

void Boxes::printDebug(){
  BOX::iterator it;
  cerr << "----BOXES---" << endl;
  cerr << "Max length: " << maxLength << " - Max width: " << width << endl;
  for (it = boxes.begin(); it < boxes.end(); it++)
    cerr << "X: " << it->first << " - Y: " << it->second << endl;
  cerr << endl;
}


void Boxes::show() const {
  INITIAL_CONF::const_iterator it;
  cout << this->getWidth() << " " << this->size() << endl;
  for (it = conf.begin(); it < conf.end(); it++)
    cout << it->first << "   " << it->second.first << " " << it->second.second << endl;
}



