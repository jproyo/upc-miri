#include <ilcplex/ilocplex.h>
ILOSTLBEGIN
int main () {
  IloEnv             env;
  IloModel     model(env);

  typedef IloArray<IloNumVarArray> NumVarMatrix;

  NumVarMatrix   mw(env, 5);
  NumVarMatrix   wg(env, 5);
  NumVarMatrix   sg(env, 5);

  const float MIN_MW [3] = {850, 1250, 1500};
  const float MAX_MW [3] = {2000, 1750, 4000};
  const float MAX_GENS [3] = {12, 10, 5};
  const float PERIOD_MW [5] = {15000, 30000, 25000, 40000, 27000};
  const float COST_MW [3] = {2, 1.3, 3};
  const float COST_START [3] = {2000, 1000, 500};
  const float COST_MIN [3] = {1000, 2600, 3000};
  const int HOURS_PERIOD[5] = {6, 3, 6, 3, 6};

  for(int i = 0; i < 5; i++){
    mw[i] = IloNumVarArray(env, 3);
    wg[i] = IloNumVarArray(env, 3);
    sg[i] = IloNumVarArray(env, 3);
    for(int j = 0; j < 3; j++){
      mw[i][j] = IloNumVar(env, ILOFLOAT);
      wg[i][j] = IloNumVar(env, 0, MAX_GENS[j], ILOINT);
      sg[i][j] = IloNumVar(env, 0, MAX_GENS[j], ILOINT);
    }
  }

  for(int i = 0; i < 5; i++){
    IloExpr demand(env);
    IloExpr extra(env);
    int before = i==0?4:i-1;
    for(int j = 0; j < 3; j++){
      demand += mw[i][j];
      extra += MAX_MW[j]*wg[i][j];
      model.add(mw[i][j] >= MIN_MW[j]*wg[i][j]);
      model.add(mw[i][j] <= MAX_MW[j]*wg[i][j]);
      model.add(sg[i][j] >= wg[i][j] - wg[before][j]);
    }
    model.add(demand >= PERIOD_MW[i]);
    model.add(extra >= 1.15*PERIOD_MW[i]);
  }

  IloExpr objExp(env);
  for(int i = 0; i < 5; i++){
    for(int j = 0; j < 3; j++){
      objExp += HOURS_PERIOD[i]*COST_MW[j]*mw[i][j] - HOURS_PERIOD[i]*COST_MW[j]*MIN_MW[j]*wg[i][j] + HOURS_PERIOD[i]*COST_MIN[j]*wg[i][j] + COST_START[j]*sg[i][j];
    }
  }
  model.add(IloMinimize(env, objExp));

  try {
    IloCplex cplex(model);
    cplex.exportModel("model.lp");
    cplex.solve();
    cout << "Min = " << cplex.getObjValue() << endl;
    cout << "--- Detailed ---" << endl;
    for(int i = 0; i < 5; i++){
      IloNumArray v(env);
      IloNumArray g(env);
      IloNumArray s(env);
      cplex.getValues(mw[i],v);
      cplex.getValues(wg[i],g);
      cplex.getValues(sg[i],s);
      cout << "-------  Period " << i+1 << "-------" << endl;
      cout << "MW Generated: " << v << endl;
      cout << "Working Generators: " << g << endl;
      cout << "Started Generators: " << s << endl;
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
