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
    IloNumVarArray x_tl(env, boxes.size(), 0, boxes.getWidth()-1, ILOINT);
    IloNumVarArray y_tl(env, boxes.size(), 0, boxes.getMaxLength()-1, ILOINT);
    IloNumVarArray x_br(env, boxes.size(), 0, boxes.getWidth()-1, ILOINT);
    IloNumVarArray y_br(env, boxes.size(), 0, boxes.getMaxLength()-1, ILOINT);
    //Boolean var to indicate rotation
    IloBoolVarArray r(env, boxes.size());
    //Length of roll
    IloNumVar l(env, 1, boxes.getMaxLength(), ILOINT);

    //Constraint 1 according to report.pdf
    x_tl[0].setBounds(0,0);
    //Constraint 2 according to report.pdf
    y_tl[0].setBounds(0,0);
    for(int i = 0; i < boxes.size(); i++){
      int width = boxes.boxWidth(i);
      int height = boxes.boxHeight(i);

      //Constraint 3 according to report.pdf
      model.add(l >= y_br[i] + 1);
      //Constraint 4 according to report.pdf
      model.add(x_tl[i] <= x_br[i]);
      //Constraint 5 according to report.pdf
      model.add(y_tl[i] <= y_br[i]);
      //Constraint 6 according to report.pdf
      model.add(x_tl[i]+(1-r[i])*(width-1)+r[i]*(height-1) == x_br[i]);
      //Constraint 7 according to report.pdf
      model.add(y_tl[i]+(1-r[i])*(height-1)+r[i]*(width-1) == y_br[i]);

      for(int j = i+1;j<boxes.size();j++){

        //Constraints 9, 10, 11 and 12 accroding to report.pdf
        model.add( x_br[i] + 1 <= x_tl[j] ||
                   y_br[i] + 1 <= y_tl[j] ||
                   x_br[j] + 1 <= x_tl[i] ||
                   y_br[j] + 1 <= y_tl[i]
                 );

      }
    }

    model.add(IloMinimize(env, l));

    IloCplex cplex(model);
    cplex.setOut(env.getNullStream());
//    cplex.exportModel("model.lp");
    if(!cplex.solve()){
      cerr << "NO SOLUTION" << endl;
      env.end();
      return 1;
    }
    boxes.show();
    cout << cplex.getObjValue() << endl;
    IloNumArray x_tl_v(env);
    IloNumArray y_tl_v(env);
    IloNumArray x_br_v(env);
    IloNumArray y_br_v(env);
    cplex.getValues(x_tl, x_tl_v);
    cplex.getValues(y_tl, y_tl_v);
    cplex.getValues(x_br, x_br_v);
    cplex.getValues(y_br, y_br_v);
    for(int i = 0; i<boxes.size(); i++){
      cout << lrint(x_tl_v[i]) << " " << lrint(y_tl_v[i]) << "  " << lrint(x_br_v[i]) << " " << lrint(y_br_v[i]) << endl;
    }
    env.end();


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
