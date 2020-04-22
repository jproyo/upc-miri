#include <ilcplex/ilocplex.h>
ILOSTLBEGIN
int main () {
  IloEnv             env;
  IloModel     model(env);

  typedef IloArray<IloNumVarArray> NumVarMatrix;

  NumVarMatrix   mw_1(env, 12);
  NumVarMatrix   mw_2(env, 10);
  NumVarMatrix   mw_3(env, 5);

  // Type 1 variables
  for(int i = 0; i < 12; i++){
    mw_1[i] = IloNumVarArray(env, 24);
    for(int j = 0; j < 24; j++){
      mw_1[i][j] = IloNumVar(env, 850, 2000, ILOINT);
    }
  }

  // Type 2 variables
  for(int i = 0; i < 10; i++){
    mw_2[i] = IloNumVarArray(env, 24);
    for(int j = 0; j < 24; j++){
      mw_2[i][j] = IloNumVar(env, 1250, 1750, ILOINT);
    }
  }

  // Type 3 variables
  for(int i = 0; i < 5; i++){
    mw_3[i] = IloNumVarArray(env, 24);
    for(int j = 0; j < 24; j++){
      mw_3[i][j] = IloNumVar(env, 1500, 4000, ILOINT);
    }
  }

  // From 12pmTo6am
  IloExpr e0To6(env);
  for(int i = 0; i < 6; i ++){
    for(int j = 0; j < 12; j++){
      e0To6 += mw_1[j][i];
    }
    for(int j = 0; j < 10; j++){
      e0To6 += mw_2[j][i];
    }
    for(int j = 0; j < 5; j++){
      e0To6 += mw_3[j][i];
    }

    model.add(e0To6 >= 15000*1.15);

  }

  // From 6amTo9am
  IloExpr e6To9(env);
  for(int i = 6; i < 9; i ++){
    for(int j = 0; j < 12; j++){
      e6To9 += mw_1[j][i];
    }
    for(int j = 0; j < 10; j++){
      e6To9 += mw_2[j][i];
    }
    for(int j = 0; j < 5; j++){
      e6To9 += mw_3[j][i];
    }
    model.add(e6To9 >= 30000*1.15);
  }

  // From 9amTo3pm
  IloExpr e9To15(env);
  for(int i = 9; i < 15; i ++){
    for(int j = 0; j < 12; j++){
      e9To15 += mw_1[j][i];
    }
    for(int j = 0; j < 10; j++){
      e9To15 += mw_2[j][i];
    }
    for(int j = 0; j < 5; j++){
      e9To15 += mw_3[j][i];
    }
    model.add(e9To15 >= 25000*1.15);
  }

  // From 3pmTo6pm
  IloExpr e15To18(env);
  for(int i = 15; i < 18; i ++){
    for(int j = 0; j < 12; j++){
      e15To18 += mw_1[j][i];
    }
    for(int j = 0; j < 10; j++){
      e15To18 += mw_2[j][i];
    }
    for(int j = 0; j < 5; j++){
      e15To18 += mw_3[j][i];
    }
    model.add(e15To18 >= 40000*1.15);
  }

  // From 6pmTo12pm
  IloExpr e18To24(env);
  for(int i = 18; i < 24; i ++){
    for(int j = 0; j < 12; j++){
      e18To24 += mw_1[j][i];
    }
    for(int j = 0; j < 10; j++){
      e18To24 += mw_2[j][i];
    }
    for(int j = 0; j < 5; j++){
      e18To24 += mw_3[j][i];
    }
    model.add(e18To24 >= 27000*1.15);
  }


  IloExpr objExp(env);
  // Type 1 variables
  for(int i = 0; i < 12; i++){
    for(int j = 0; j < 24; j++){
      objExp += 1000 + 2*(mw_1[i][j]-850);
    }
  }

  // Type 2 variables
  for(int i = 0; i < 10; i++){
    for(int j = 0; j < 24; j++){
      objExp += 1250 + 1.3*(mw_2[i][j]-1250);
    }
  }

  // Type 3 variables
  for(int i = 0; i < 5; i++){
    for(int j = 0; j < 24; j++){
      objExp += 1500 + 3*(mw_3[i][j]-1500);
    }
  }

  model.add(IloMinimize(env, objExp + 36500));
  try {
    IloCplex cplex(model);
  cplex.exportModel("model.lp");
    cplex.solve();
    cout << "Min = " << cplex.getObjValue() << endl;
    cout << "--- Type 1 ---" << endl;
    for(int i = 0; i < 12; i++){
      IloNumArray v(env);
      cplex.getValues(mw_1[i],v);
      cout << "Generator " << i+1 << " - Power: " << v << endl;
    }
    cout << "--- Type 2 ---" << endl;
    for(int i = 0; i < 10; i++){
      IloNumArray v(env);
      cplex.getValues(mw_2[i],v);
      cout << "Generator " << i+1 << " - Power: " << v << endl;
    }
    cout << "--- Type 3 ---" << endl;
    for(int i = 0; i < 5; i++){
      IloNumArray v(env);
      cplex.getValues(mw_3[i],v);
      cout << "Generator " << i+1 << " - Power: " << v << endl;
    }
    env.end();
  } catch (IloAlgorithm::CannotExtractException &e) {
     IloExtractableArray &failed = e.getExtractables();
     cerr << "Failed to extract:" << endl;
     for (IloInt i = 0; i < failed.getSize(); ++i)
        cerr << "\t" << failed[i] << endl;
  } catch (IloException &e ) {
  }
}
