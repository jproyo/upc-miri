from confmat import ConfMat
from naivebayes import NaiveBayes
from maxapost import MaxAPost
from data import Data

filename = "datasets/titanicTr.txt"
##filename = "haireyescolor.txt"
##filename = "datasets/cmcTr.txt"
##filename = "datasets/lensesTr.txt"

d = Data(filename)

prmap = MaxAPost(d)
prmap.train()

prnb = NaiveBayes(d)
prnb.train()

cmmap = ConfMat(prmap.clsscnts)
cmnb = ConfMat(prnb.clsscnts)
comparing = set([])
for (v,c_true) in d.test_set:
    c_pred_map = tuple(prmap.predict(v))
    c_pred_nb = tuple(prnb.predict(v))
    if len(c_pred_map) and len(c_pred_nb):
        warn = (c_pred_map[0] != c_pred_nb[0])
        cmmap.mat[c_pred_map[0],c_true] += 1
        cmnb.mat[c_pred_nb[0],c_true] += 1
    else:
        warn = True
    if warn:
        comparing.add((v,c_true,c_pred_map,c_pred_nb))

print()
for r in sorted(comparing):
    print(r[0], ": true class ", r[1])
    print("    MAP pred", r[2], end = '')
    print("    NB pred", r[3])

##prmap.show()
##prnb.show()
##print("MAP:")
##cmmap.report()
##print("NB:")
##cmnb.report()
