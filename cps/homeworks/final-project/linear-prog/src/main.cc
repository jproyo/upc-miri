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
    //Length of roll
    IloNumVar l(env, 1, boxes.getMaxLength(), ILOINT);


    // Setting bounds
    for(int i = 0; i < boxes.size(); i++){
      x_tl[i] = IloNumVar(env, 0, boxes.getWidth()-1, ILOINT);
      y_tl[i] = IloNumVar(env, 0, boxes.getMaxLength()-1, ILOINT);
      x_br[i] = IloNumVar(env, 0, boxes.getWidth()-1, ILOINT);
      y_br[i] = IloNumVar(env, 0, boxes.getMaxLength()-1, ILOINT);
      r[i] = IloBoolVar(env);
    }

    //Constraint 10 according to report.pdf
    x_tl[0].setBounds(0,0);
    //Constraint 12 according to report.pdf
    y_tl[0].setBounds(0,0);
    for(int i = 0; i < boxes.size(); i++){
      int width = boxes.boxWidth(i);
      int height = boxes.boxHeight(i);
      //Constraint 2 according to report.pdf
      model.add(x_tl[i] <= x_br[i]);
      //Constraint 3 according to report.pdf
      model.add(y_tl[i] <= y_br[i]);
      //Constraint 4 according to report.pdf
      model.add(x_tl[i]+(1-r[i])*(width-1)+r[i]*(height-1) == x_br[i]);
      //Constraint 5 according to report.pdf
      model.add(y_tl[i]+(1-r[i])*(height-1)+r[i]*(width-1) == y_br[i]);

      for(int j = i+1;j<boxes.size();j++){
        int width_j = boxes.boxWidth(j);
        int height_j = boxes.boxHeight(j);

        //Constraint 6 according to report.pdf
        model.add(
            (x_tl[i] + (1-r[i])*width + r[i]*height <= x_tl[j]) ||
            (y_tl[i] + (1-r[i])*height + r[i]*width <= y_tl[j]) ||
            (x_tl[j] + (1-r[j])*width_j + r[j]*height_j <= x_tl[i]) ||
            (y_tl[j] + (1-r[j])*height_j + r[j]*width_j <= y_tl[i]));

      }
      //Constraint 1 according to report.pdf
      model.add(l >= y_tl[i] + (1-r[i])*height + r[i]*width);
    }

    model.add(IloMinimize(env, l));

    IloCplex cplex(model);
    cplex.exportModel("model.lp");
    if(!cplex.solve()){
      cerr << "NO SOLUTION" << endl;
      env.end();
      return 1;
    }
    cerr << "Min = " << cplex.getObjValue() << endl;
    cerr << "--- Detailed ---" << endl;
    IloNumArray x_tl_v(env);
    IloNumArray y_tl_v(env);
    IloNumArray x_br_v(env);
    IloNumArray y_br_v(env);
    cplex.getValues(x_tl, x_tl_v);
    cplex.getValues(y_tl, y_tl_v);
    cplex.getValues(x_br, x_br_v);
    cplex.getValues(y_br, y_br_v);
    for(int i = 0; i<boxes.size(); i++){
      cerr << "-------  Box " << i+1 << "-------" << endl;
      cerr << lrint(x_tl_v[i]) << " " << lrint(y_tl_v[i]) << "  " << lrint(x_br_v[i]) << " " << lrint(y_br_v[i]) << endl;
    }
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
  } catch (...) {
    cerr << "Error" << endl;
    return 1;
  }

}
