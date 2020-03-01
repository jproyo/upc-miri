
from random import randint, seed

class Data:

    def __init__(self,filename,justrain=0):
        """
        justrain == 0: use training data as test data
        justrain == p > 0: use approx p% as training data and rest as test
        """
        seed(9254187)
        self.N = 0
        self.filename = filename
        self.justrain = justrain
        self.headers = []
        self.training_set = []
        self.test_set = []
        datafile = open(filename)
        header = True
        for line in datafile:
            if header:
                header = False
                self.headers = line.split()
                for el in self.headers:
                    el.strip()
            else:
                self.N += 1
                ls = line.split()
                for el in ls: 
                    el.strip()
                clss = ls[-1]
                values = tuple(ls[:-1])
                if justrain == 0 or randint(1,100) <= justrain:
                    self.training_set.append((values,clss))
                else:
                    self.test_set.append((values,clss))
        datafile.close()
        if justrain == 0:
            self.test_set = self.training_set

    def report(self):
        print(self.N, "observations from file", self.filename)
        if self.justrain == 0:
            print("testing with whole training set")
        else:
            print("training with", len(self.training_set), "observations")
            print("testing with", len(self.test_set), "observations")

if __name__=="__main__":
    "should add some further testing here"

    filename = "datasets/titanicTr.txt"

    print(filename)
    
    d = Data(filename)
    print(d.N)
    print(len(d.training_set))
    print(len(d.test_set))

    d = Data(filename,75)
    print(d.N)
    print(len(d.training_set))
    print(len(d.test_set))

