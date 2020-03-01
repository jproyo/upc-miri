"""
Loads in a predictor, trains it with the data in
the datafile (in transactional form) and plots the
ROC curve for the chosen positive prediction pos_class

print_numbers controls whether a lot of numeric info
is dumped into the console

current notion of result.append tailored to NB
"""

import matplotlib.pyplot as plt

class Roc:
    "draws a ROC curve for a predictor and a dataset"
    
    def __init__(self, predictor, target_class):
        "predictor must bring data link inside and come already trained"
        self.set_pred(predictor)
        self.retarget(target_class)

    def retarget(self,target_class):
        self.t = target_class
        self.curve = None

    def set_pred(self, predictor):
        self.pr = predictor
        self.preds = None

    def do_curve(self):
        "do curve = (x,y), each a list of floats in [0,1]"
        pos = 0.0
        neg = 0.0
        for (v,c_true) in self.pr.data.test_set:
            "count true pos and true neg cases"
            if c_true == self.t: 
                pos += 1
            else:
                neg += 1
        self._all_preds(self.pr.data)
        trpos = 0
        fapos = 0
        x = [0.0]
        y = [0.0]
        for e in self.preds:
            if e[3] == self.t: 
                trpos += 1
            else: 
                fapos += 1
            x.append(fapos/neg)
            y.append(trpos/pos)
        self.curve = (x,y,pos,neg)

    def _all_preds(self,d):
        "compute all predictions if necessary"
        if self.preds is not None: return
        self.preds = []
        pr = self.pr
        cnt = 0
        for (v,c_true) in d.test_set:
            """
            prepare predictions for sorting
            if the cnt increment is omitted, then,
                in case of equal weight, positive instances will come first
            store both true class and first prediction
            """
            cnt +=1 # omit to reorder
            c_pred = pr.predict(v)[0]
            wy = 0
            wn = 0
            for c in pr.clssprobs:
                if c == self.t: 
                    wy += pr.value_weight(v,c)
                else: 
                    wn += pr.value_weight(v,c)
            self.preds.append((wy/(wy+wn),cnt,c_true==self.t,c_true,c_pred))
#ALT: append wy or wy-wn instead of wy/(wy+wn)
            self.preds = sorted(self.preds,reverse=True)
    
    def draw_curve(self):
        if self.curve is None:
            self.do_curve()
        plt.plot([-0.001,1.001],[-0.001,1.001],color="orange") # diagonal reference
        plt.plot(self.curve[0],self.curve[1])
        plt.axes().set_aspect('equal')
        plt.xlabel("False positive rate")
        plt.ylabel("True positive rate")
        plt.show()

if __name__ == "__main__":

    from naivebayes import NaiveBayes
    from data import Data

    print_numbers = False

    datafile = "datasets/titanicTr.txt"
    pos_class = "Survived:Yes"
##pos_class = "Survived:No"

##datafile = "datasets/cmcTr.txt"
##pos_class = "contraceptive-method:none"

    d = Data(datafile)

    prnb = NaiveBayes(d)
    prnb.train()

    r = Roc(prnb,pos_class)

    r.do_curve()

    print("Predicting", pos_class, "for data file", datafile, end = ' ')
    print("with", int(r.curve[2]), "positive instances and", int(r.curve[3]), "negative instances")

    if print_numbers:
        prnb.show()

        print("Scores for predicting", pos_class, ":")
        for e in sorted(r.preds): 
            print(e)
        print("===")
        print("Curve coordinates:")
        for e in zip(r.curve[0],r.curve[1]): 
            print(e)

    r.draw_curve()
