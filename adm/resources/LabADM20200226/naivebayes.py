
from collections import defaultdict
from data import Data

class NaiveBayes:

    def __init__(self,data):
        self.data = data
        self.normalized = False
        self.clsscnts = defaultdict(int)
        self.condcnts = defaultdict(int)
        self.attrcnts = defaultdict(int)
        self.condprobs = {}
        self.clssprobs = {}

    def train(self):
        self.normalized = False
        for (v,c) in self.data.training_set:
            self.clsscnts[c] += 1
            for attr in v:
                self.attrcnts[attr] += 1
                self.condcnts[attr,c] += 1
        self.normalize()

    def normalize(self):
        if self.normalized: return
        self.normalized = True
        for a in self.attrcnts:
            for c in self.clsscnts:
                self.condprobs[a,c] = \
                    float(self.condcnts[a,c])/self.clsscnts[c]
        for c in self.clsscnts:
            self.clssprobs[c] = float(self.clsscnts[c])/self.data.N

    def value_weight(self,attrs,clval):
        "weight of class value clval for these attrs"
        prc = self.clssprobs[clval]
        for attr in attrs:
            prc *= self.condprobs[attr,clval]
        return prc

# missing method float value prediction

    def predict(self,attrs):
        predictions = []
        mx = 0.0
        for c in self.clssprobs.keys():
            prc = self.value_weight(attrs,c)
            if prc > mx:
                predictions = []
            if prc >= mx:
                mx = prc
                predictions.append(c)
        return predictions

    def show(self):
        print("N =", self.data.N)
        print("\nclass probs:")
        for c in self.clssprobs:
            print(c, self.clssprobs[c])
        print("\nattr probs:")
        for c in self.clssprobs:
            print("\nclass", c, ":")
            for a in sorted(self.attrcnts):
                print(a, self.condprobs[a,c])

if __name__=="__main__":

    filename = \
    "datasets/titanicTr.txt"
#    "weatherNominalTr.txt"
    d = Data(filename)

    d.report()

    pr = NaiveBayes(d)
    pr.train()
    pr.show()
    # exit()



    from confmat import ConfMat

    cm = ConfMat(pr.clsscnts)
    print()
    for (v,c_true) in d.test_set:
        c_pred = pr.predict(v)[0]
        print(v, c_pred, "( true class:", c_true, ")")
        cm.mat[c_pred,c_true] += 1
    print()
    pr.show()
    print()
    cm.report()

    print(pr.predict(("Class:1st","Sex:Female","Age:Child")))

    print(pr.predict(("Class:Crew","Sex:Female","Age:Child")))
