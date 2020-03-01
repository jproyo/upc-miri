
from collections import defaultdict
from data import Data

class MaxAPost:

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
            self.attrcnts[v] += 1
            self.condcnts[v,c] += 1
        self.normalize()

    def normalize(self):
        if self.normalized: return
        self.normalized = True
        for vals in self.attrcnts:
            for c in self.clsscnts:
                self.condprobs[vals,c] = \
                    float(self.condcnts[vals,c])/self.attrcnts[vals]
        for c in self.clsscnts:
            self.clssprobs[c] = float(self.clsscnts[c])/self.data.N

    def value_weight(self,attrs,clval):
        "weight of class value clval for these attrs"
        return self.condprobs[attrs,clval]

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
##    "datasets/weatherNominalTr.txt"
    d = Data(filename)

    d.report()

    pr = MaxAPost(d)
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

    # exit()


    print(pr.predict(("Class:1st","Sex:Female","Age:Child")))
    print(pr.predict(("Class:1st","Sex:Male","Age:Adult")))

    #print(pr.predict(("Class:Crew","Sex:Female","Age:Child")))
