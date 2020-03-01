class ConfMat:
    """
    Confusion matrix:
    predictions: first (horizontal) coordinate
    true class labels: second (vertical) coordinate
    direct use such as: confmatobject.mat[predlab,truelab] += 1
    Made for Python2 that's why it does not extend dict, some day it should
    """
    
    def __init__(self,classvals):
        self.classvals = sorted(classvals)
        self.mat = {}
        for c_pred in self.classvals:
            for c_true in self.classvals:
                self.mat[c_pred,c_true] = 0

    def report(self):
        "print off the confusion matrix"

        colsepspaces = 3
        m = colsepspaces
        for c in self.classvals:
            if len(str(c)) > m:
                m = len(str(c))
        m += colsepspaces
        format_string = "%%%ds" % m

        print("Predictions:")
        for c in self.classvals:
            print(format_string % str(c), end = ' ')
        print(format_string % " True labels:")

        for c_true in self.classvals:
            for c_pred in self.classvals:
                print(format_string % str(self.mat[c_pred,c_true]), end = ' ')
            print(format_string % c_true)

        cnt = 0
        for c in self.classvals:
            cnt += self.mat[c,c]
        print(cnt, "instances correctly predicted")

            
