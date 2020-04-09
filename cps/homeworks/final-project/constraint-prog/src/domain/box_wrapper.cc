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

class Boxes {

  private:
    int width;
    int maxLength;
    BOX boxes;

    Boxes(int w, const BOX& b): width(w), boxes(b){
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
    void print();

  static Boxes& fromStdIn(){
    BOX bs;
    int w;
    int num;
    cin >> w >> num;
    int n, x, y;
    int c = 1;
    while (num != 0) {
      cin >> n >> x >> y;
      num -= n;
      for(int i = 0; i < n; i++){
        bs.push_back(DIM(x,y));
      }
      ++c;
    }

    static Boxes b(w,bs);
    return b;
  }
};

Boxes::Boxes(){}

Boxes::Boxes(const Boxes& b2): width(b2.width), maxLength(b2.maxLength), boxes(b2.boxes){}

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

void Boxes::print(){
  BOX::iterator it;
  cerr << "----BOXES---" << endl;
  cerr << "Max length: " << maxLength << " - Max width: " << width << endl;
  for (it = boxes.begin(); it < boxes.end(); it++)
    cerr << "X: " << it->first << " - Y: " << it->second << endl;
  cerr << endl;


  cerr << "Size: " << this->size() << endl;
}



