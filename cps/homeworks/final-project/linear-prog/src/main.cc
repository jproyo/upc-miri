/**
 *
 * This class contains the main entry point of the program and the Space subclass with constraint
 *
 * @author: Juan Pablo Royo Sales
 * @date: April 6th, 2020
 * @subject: CPS
 * @institution: Universitat Politècnica Catalunya
 * @title: Final Project
 *
 */
#include <cstdlib>
#include <vector>
#include <ilcplex/ilocplex.h>

#ifndef UTILS
#define UTILS
#include "utils/utils.cpp"
#endif

using namespace utils;

ILOSTLBEGIN
int main(int argc, char* argv[]) {

  show_help(argc,argv);

  try{

    Boxes boxes = Boxes::fromStdIn();

    boxes.printDebug();

    IloEnv             env;
    IloModel     model(env);

    //Position variables
    IloNumVarArray x_tl(env, boxes.size());
    IloNumVarArray y_tl(env, boxes.size());
    IloNumVarArray x_br(env, boxes.size());
    IloNumVarArray y_br(env, boxes.size());
    //Boolean var to indicate rotation
    IloBoolVarArray r(env, boxes.size());
    //Boolean var to indicate left most position on each box
    IloBoolVarArray l(env, boxes.size());

    for(int i = 0; i < boxes.size(); i++){
      x_tl[i] = IloNumVar(env, 0, boxes.getWidth()-1, ILOINT);
      y_tl[i] = IloNumVar(env, 0, boxes.getMaxLength()-1, ILOINT);
      x_br[i] = IloNumVar(env, 0, boxes.getWidth()-1, ILOINT);
      y_br[i] = IloNumVar(env, 0, boxes.getMaxLength()-1, ILOINT);
      r[i] = IloBoolVar(env);
      l[i] = IloBoolVar(env);
    }

    for(int i = 0; i < boxes.size(); i++){
      int width = boxes.boxWidth(i);
      int height = boxes.boxHeight(i);
      model.add(x_tl[i] + l[i] <= 1);
      model.add(x_tl[i] + (1-l[i]) > 1);
      model.add(x_tl[i] <= x_br[i]);
      model.add(y_tl[i] <= y_br[i]);
      model.add(x_br[i] <= width-1);
      model.add(x_tl[i]+r[i]*(width-1)+(1-r[i])*(height-1) == x_br[i]);
      model.add(y_tl[i]+r[i]*(height-1)+(1-r[i])*(width-1) == y_br[i]);

      for(int j = i+1;j<boxes.size();j++){
        int area_i = boxes.boxArea(i);
        int area_j = boxes.boxArea(j);
        if(area_i < area_j){
          model.add(x_tl[j] - x_tl[i] + r[i]*(width-1) + (1-r[i])*(height-1) > 0);
          model.add(y_tl[j] - y_tl[i] + r[i]*(height-1) + (1-r[i])*(width-1) > 0);
        }
        if(area_i > area_j){
          model.add(x_tl[i] < x_tl[j]);
          model.add(y_tl[i] < y_tl[j]);
        }

      }
    }

    IloExpr objExp(env);
    for(int i = 0; i < boxes.size(); i++){
      objExp += l[i] * ((x_br[i] - x_tl[i]) + 1);
    }
    model.add(IloMinimize(env, objExp));

    IloCplex cplex(model);
    cplex.exportModel("model.lp");
    cplex.solve();
    cout << "Min = " << cplex.getObjValue() << endl;
//    cout << "--- Detailed ---" << endl;
//    for(int i = 0; i < 5; i++){
//      IloNumArray v(env);
//      IloNumArray g(env);
//      IloNumArray s(env);
//      cplex.getValues(mw[i],v);
//      cplex.getValues(wg[i],g);
//      cplex.getValues(sg[i],s);
//      cout << "-------  Period " << i+1 << "-------" << endl;
//      cout << "MW Generated: " << v << endl;
//      cout << "Working Generators: " << g << endl;
//      cout << "Started Generators: " << s << endl;
//    }
    env.end();

//    show_result(boxes, newSol);


  } catch (IloAlgorithm::CannotExtractException &e) {
     IloExtractableArray &failed = e.getExtractables();
     cerr << "Failed to extract:" << endl;
     for (IloInt i = 0; i < failed.getSize(); ++i)
        cerr << "\t" << failed[i] << endl;
     return 1;
  } catch (IloException &e ) {
    cerr << "Exception: " << e.getMessage() << endl;
    return 1;
  }
}
