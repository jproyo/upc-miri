#include <ilcplex/ilocplex.h>
ILOSTLBEGIN
int main () {
  IloEnv             env;
  IloModel     model(env);
  IloNumVarArray   x(env);
  x.add(IloNumVar(env, 0, 40));
  x.add(IloNumVar(env));//default: between $0$ and $+\infty$ 
  x.add(IloNumVar(env));
  model.add( - x[0] +     x[1] + x[2] <= 20);
  model.add(   x[0] - 3 * x[1] + x[2] <= 30);
  model.add(IloMaximize(env, x[0]+2*x[1]+3*x[2]));
  IloCplex cplex(model);
  cplex.solve();
  cout << "Max=" << cplex.getObjValue() << endl;
  env.end();
}
