# This file has the following format
# TYPE_RESOURCE NODE_ID FROM_STEP TO_STEP CONNECTED_TO_NODE_ID
ASAP
M 1 1 1 [2]
M 2 2 2 [3]
S 3 3 3 [4]
S 4 4 4
M 5 1 1 [2]
M 6 1 1 [7]
M 7 2 3 [4]
M 8 1 1 [9]
A 9 2 2
A 10 1 1 [11]
C 11 2 2
ALAP
M 1 1 1 [2]
M 2 2 2 [3]
S 3 3 3 [4]
S 4 4 4
M 5 1 1 [2]
M 6 2 2 [7]
M 7 3 3 [4]
M 8 3 3 [9]
A 9 4 4
A 10 3 3 [11]
C 11 4 4
RESOURCES
M 2 3
A 1 3
S 1 3
C 1 3
