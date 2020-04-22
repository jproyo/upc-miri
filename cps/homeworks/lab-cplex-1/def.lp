min

  12000 * hm_1 + 26000 * hm_2 + 15000 * hm_3
    + 24 * ckw_1 + 13 * ckw_2 + 15 * ckw_3
    + 36500 - 988540

st

 12pmTo6am: 2000 * ckw_1 + 1750 * ckw_2 + 4000 * ckw_3 <= 15000
 12pmTo6amMin: 850 * hm_1 + 1250 * hm_2 + 1500 * hm_3 <= 15000


 6amTo9am: 2000 * ckw_1 + 1750 * ckw_2 + 4000 * ckw_3 <= 30000
 6amTo9amMin: 850 * hm_1 + 1250 * hm_2 + 1500 * hm_3 <= 30000


 9amTo3pm: 2000 * ckw_1 + 1750 * ckw_2 + 4000 * ckw_3 <= 25000
 9amTo3pmMin: 850 * hm_1 + 1250 * hm_2 + 1500 * hm_3 <= 25000


 3pmTo6pm: 2000 * ckw_1 + 1750 * ckw_2 + 4000 * ckw_3 <= 40000
 3pmTo6pmMin: 850 * hm_1 + 1250 * hm_2 + 1500 * hm_3 <= 40000

 6pmTo12pm: 2000 * ckw_1 + 1750 * ckw_2 + 4000 * ckw_3 <= 27000
 6pmTo12pmMin: 850 * hm_1 + 1250 * hm_2 + 1500 * hm_3 <= 27000

hm_i = Amount hours working at min
ckw_i = Amount kw/hours working above min
36500 = Startup cost

---------------------------------------------------


vars:

for each reactor i have 24 vars representing hour with it type

mw_i_j_h  = amount mw in reactor j type i hour h


min x - 988540

  sum_{i=1}^12 sum_{j=1}^3 sum_{h=0}^24 mw_1_j_h * 2 .... +  36500 - 988540

st

 12pmTo6am:
 sum_{j=1}^12 sum_{h=0}^6 mw_1_j_h + sum_{j=1}^10 sum_{h=0}^6 mw_2_j_h + sum_{j=1}^5 sum_{h=0}^6  mw_3_j_h <= 15000

 6amTo9am: idem change index h <= 30000
 9amTo3pm: idem change index h <= 25000
 3pmTo6pm: idem change index h <= 40000
 6pmTo12pm: idem change index h <= 27000


 850 <= mw_1_j_h <= 2000
 1250 <= mw_2_j_h <= 1750
 1500 <= mw_3_j_h <= 4000





