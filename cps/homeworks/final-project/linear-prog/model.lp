\ENCODING=ISO-8859-1
\Problem name: IloCplex

Minimize
 obj1: x3 + 0 x36 + 0 x41
Subject To
 c1:   - x1 + x2 + x3 >= 4
 c2:   x4 - x5 <= 0
 c3:   x1 - x6 <= 0
 c4:   x2 + x4 - x5  = -2
 c5:   x1 - x2 - x6  = -3
 c6:   x2 + x4 - x7 - 3 x8 - 3 x9 <= -3
 c7:   x1 - x2 - 34 x8 + 34 x9 - x10 <= 30
 c8:   - x4 + x7 + 3 x8 - 3 x9 + x11 <= 0
 c9:   - x1 + 34 x8 + 34 x9 + x10 - x11 <= 64
 c10:  x2 + x4 - x12 - 3 x13 - 3 x14 <= -3
 c11:  x1 - x2 - 34 x13 + 34 x14 - x15 <= 30
 c12:  - x4 + x12 + 3 x13 - 3 x14 + x16 <= 0
 c13:  - x1 + 34 x13 + 34 x14 + x15 - x16 <= 64
 c14:  x2 + x4 - x17 - 3 x18 - 3 x19 <= -3
 c15:  x1 - x2 - 34 x18 + 34 x19 - x20 <= 30
 c16:  - x4 + x17 + 3 x18 - 3 x19 + x21 <= 0
 c17:  - x1 + 34 x18 + 34 x19 + x20 - x21 <= 64
 c18:  x2 + x4 - x22 - 3 x23 - 3 x24 <= -3
 c19:  x1 - x2 - 34 x23 + 34 x24 - x25 <= 30
 c20:  - x4 + x22 + 3 x23 - 3 x24 + x26 <= 0
 c21:  - x1 + 34 x23 + 34 x24 + x25 - x26 <= 64
 c22:  x2 + x4 - x27 - 3 x28 - 3 x29 <= -3
 c23:  x1 - x2 - 34 x28 + 34 x29 - x30 <= 30
 c24:  - x4 + x27 + 3 x28 - 3 x29 + x31 <= 0
 c25:  - x1 + 34 x28 + 34 x29 + x30 - x31 <= 64
 c26:  x2 + x4 - x32 - 3 x33 - 3 x34 <= -3
 c27:  x1 - x2 - 34 x33 + 34 x34 - x35 <= 30
 c28:  - x4 + x32 + 3 x33 - 3 x34 <= 1
 c29:  - x1 + 34 x33 + 34 x34 + x35 <= 66
 c30:  x2 + x4 - x37 - 3 x38 - 3 x39 <= -3
 c31:  x1 - x2 - 34 x38 + 34 x39 - x40 <= 30
 c32:  - x4 + x37 + 3 x38 - 3 x39 <= 1
 c33:  - x1 + 34 x38 + 34 x39 + x40 <= 66
 c34:  x2 + x4 - x42 - 3 x43 - 3 x44 <= -3
 c35:  x1 - x2 - 34 x43 + 34 x44 - x45 <= 30
 c36:  - x4 + x42 + 3 x43 - 3 x44 + x46 <= 2
 c37:  - x1 + 34 x43 + 34 x44 + x45 - x46 <= 66
 c38:  x2 + x4 - x47 - 3 x48 - 3 x49 <= -3
 c39:  x1 - x2 - 34 x48 + 34 x49 - x50 <= 30
 c40:  - x4 + x47 + 3 x48 - 3 x49 + x51 <= 2
 c41:  - x1 + 34 x48 + 34 x49 + x50 - x51 <= 66
 c42:  x2 + x4 - x52 - 3 x53 - 3 x54 <= -3
 c43:  x1 - x2 - 34 x53 + 34 x54 - x55 <= 30
 c44:  - x4 + x52 + 3 x53 - 3 x54 + x56 <= 2
 c45:  - x1 + 34 x53 + 34 x54 + x55 - x56 <= 66
 c46:  x3 - x10 + x11 >= 4
 c47:  x7 - x57 <= 0
 c48:  x10 - x58 <= 0
 c49:  x7 + x11 - x57  = -2
 c50:  x10 - x11 - x58  = -3
 c51:  x7 + x11 - x12 - 3 x18 - 3 x19 <= -3
 c52:  x10 - x11 - x15 - 34 x18 + 34 x19 <= 30
 c53:  - x7 + x12 + x16 + 3 x18 - 3 x19 <= 0
 c54:  - x10 + x15 - x16 + 34 x18 + 34 x19 <= 64
 c55:  x7 + x11 - x17 - 3 x23 - 3 x24 <= -3
 c56:  x10 - x11 - x20 - 34 x23 + 34 x24 <= 30
 c57:  - x7 + x17 + x21 + 3 x23 - 3 x24 <= 0
 c58:  - x10 + x20 - x21 + 34 x23 + 34 x24 <= 64
 c59:  x7 + x11 - x22 - 3 x28 - 3 x29 <= -3
 c60:  x10 - x11 - x25 - 34 x28 + 34 x29 <= 30
 c61:  - x7 + x22 + x26 + 3 x28 - 3 x29 <= 0
 c62:  - x10 + x25 - x26 + 34 x28 + 34 x29 <= 64
 c63:  x7 + x11 - x27 - 3 x33 - 3 x34 <= -3
 c64:  x10 - x11 - x30 - 34 x33 + 34 x34 <= 30
 c65:  - x7 + x27 + x31 + 3 x33 - 3 x34 <= 0
 c66:  - x10 + x30 - x31 + 34 x33 + 34 x34 <= 64
 c67:  x7 + x11 - x32 - 3 x38 - 3 x39 <= -3
 c68:  x10 - x11 - x35 - 34 x38 + 34 x39 <= 30
 c69:  - x7 + x32 + 3 x38 - 3 x39 <= 1
 c70:  - x10 + x35 + 34 x38 + 34 x39 <= 66
 c71:  x7 + x11 - x37 - 3 x43 - 3 x44 <= -3
 c72:  x10 - x11 - x40 - 34 x43 + 34 x44 <= 30
 c73:  - x7 + x37 + 3 x43 - 3 x44 <= 1
 c74:  - x10 + x40 + 34 x43 + 34 x44 <= 66
 c75:  x7 + x11 - x42 - 3 x48 - 3 x49 <= -3
 c76:  x10 - x11 - x45 - 34 x48 + 34 x49 <= 30
 c77:  - x7 + x42 + x46 + 3 x48 - 3 x49 <= 2
 c78:  - x10 + x45 - x46 + 34 x48 + 34 x49 <= 66
 c79:  x7 + x11 - x47 - 3 x53 - 3 x54 <= -3
 c80:  x10 - x11 - x50 - 34 x53 + 34 x54 <= 30
 c81:  - x7 + x47 + x51 + 3 x53 - 3 x54 <= 2
 c82:  - x10 + x50 - x51 + 34 x53 + 34 x54 <= 66
 c83:  x7 + x11 - x52 - 3 x59 - 3 x60 <= -3
 c84:  x10 - x11 - x55 - 34 x59 + 34 x60 <= 30
 c85:  - x7 + x52 + x56 + 3 x59 - 3 x60 <= 2
 c86:  - x10 + x55 - x56 + 34 x59 + 34 x60 <= 66
 c87:  x3 - x15 + x16 >= 4
 c88:  x12 - x61 <= 0
 c89:  x15 - x62 <= 0
 c90:  x12 + x16 - x61  = -2
 c91:  x15 - x16 - x62  = -3
 c92:  x12 + x16 - x17 - 3 x28 - 3 x29 <= -3
 c93:  x15 - x16 - x20 - 34 x28 + 34 x29 <= 30
 c94:  - x12 + x17 + x21 + 3 x28 - 3 x29 <= 0
 c95:  - x15 + x20 - x21 + 34 x28 + 34 x29 <= 64
 c96:  x12 + x16 - x22 - 3 x33 - 3 x34 <= -3
 c97:  x15 - x16 - x25 - 34 x33 + 34 x34 <= 30
 c98:  - x12 + x22 + x26 + 3 x33 - 3 x34 <= 0
 c99:  - x15 + x25 - x26 + 34 x33 + 34 x34 <= 64
 c100: x12 + x16 - x27 - 3 x38 - 3 x39 <= -3
 c101: x15 - x16 - x30 - 34 x38 + 34 x39 <= 30
 c102: - x12 + x27 + x31 + 3 x38 - 3 x39 <= 0
 c103: - x15 + x30 - x31 + 34 x38 + 34 x39 <= 64
 c104: x12 + x16 - x32 - 3 x43 - 3 x44 <= -3
 c105: x15 - x16 - x35 - 34 x43 + 34 x44 <= 30
 c106: - x12 + x32 + 3 x43 - 3 x44 <= 1
 c107: - x15 + x35 + 34 x43 + 34 x44 <= 66
 c108: x12 + x16 - x37 - 3 x48 - 3 x49 <= -3
 c109: x15 - x16 - x40 - 34 x48 + 34 x49 <= 30
 c110: - x12 + x37 + 3 x48 - 3 x49 <= 1
 c111: - x15 + x40 + 34 x48 + 34 x49 <= 66
 c112: x12 + x16 - x42 - 3 x53 - 3 x54 <= -3
 c113: x15 - x16 - x45 - 34 x53 + 34 x54 <= 30
 c114: - x12 + x42 + x46 + 3 x53 - 3 x54 <= 2
 c115: - x15 + x45 - x46 + 34 x53 + 34 x54 <= 66
 c116: x12 + x16 - x47 - 3 x59 - 3 x60 <= -3
 c117: x15 - x16 - x50 - 34 x59 + 34 x60 <= 30
 c118: - x12 + x47 + x51 + 3 x59 - 3 x60 <= 2
 c119: - x15 + x50 - x51 + 34 x59 + 34 x60 <= 66
 c120: x12 + x16 - x52 - 3 x63 - 3 x64 <= -3
 c121: x15 - x16 - x55 - 34 x63 + 34 x64 <= 30
 c122: - x12 + x52 + x56 + 3 x63 - 3 x64 <= 2
 c123: - x15 + x55 - x56 + 34 x63 + 34 x64 <= 66
 c124: x3 - x20 + x21 >= 4
 c125: x17 - x65 <= 0
 c126: x20 - x66 <= 0
 c127: x17 + x21 - x65  = -2
 c128: x20 - x21 - x66  = -3
 c129: x17 + x21 - x22 - 3 x38 - 3 x39 <= -3
 c130: x20 - x21 - x25 - 34 x38 + 34 x39 <= 30
 c131: - x17 + x22 + x26 + 3 x38 - 3 x39 <= 0
 c132: - x20 + x25 - x26 + 34 x38 + 34 x39 <= 64
 c133: x17 + x21 - x27 - 3 x43 - 3 x44 <= -3
 c134: x20 - x21 - x30 - 34 x43 + 34 x44 <= 30
 c135: - x17 + x27 + x31 + 3 x43 - 3 x44 <= 0
 c136: - x20 + x30 - x31 + 34 x43 + 34 x44 <= 64
 c137: x17 + x21 - x32 - 3 x48 - 3 x49 <= -3
 c138: x20 - x21 - x35 - 34 x48 + 34 x49 <= 30
 c139: - x17 + x32 + 3 x48 - 3 x49 <= 1
 c140: - x20 + x35 + 34 x48 + 34 x49 <= 66
 c141: x17 + x21 - x37 - 3 x53 - 3 x54 <= -3
 c142: x20 - x21 - x40 - 34 x53 + 34 x54 <= 30
 c143: - x17 + x37 + 3 x53 - 3 x54 <= 1
 c144: - x20 + x40 + 34 x53 + 34 x54 <= 66
 c145: x17 + x21 - x42 - 3 x59 - 3 x60 <= -3
 c146: x20 - x21 - x45 - 34 x59 + 34 x60 <= 30
 c147: - x17 + x42 + x46 + 3 x59 - 3 x60 <= 2
 c148: - x20 + x45 - x46 + 34 x59 + 34 x60 <= 66
 c149: x17 + x21 - x47 - 3 x63 - 3 x64 <= -3
 c150: x20 - x21 - x50 - 34 x63 + 34 x64 <= 30
 c151: - x17 + x47 + x51 + 3 x63 - 3 x64 <= 2
 c152: - x20 + x50 - x51 + 34 x63 + 34 x64 <= 66
 c153: x17 + x21 - x52 - 3 x67 - 3 x68 <= -3
 c154: x20 - x21 - x55 - 34 x67 + 34 x68 <= 30
 c155: - x17 + x52 + x56 + 3 x67 - 3 x68 <= 2
 c156: - x20 + x55 - x56 + 34 x67 + 34 x68 <= 66
 c157: x3 - x25 + x26 >= 4
 c158: x22 - x69 <= 0
 c159: x25 - x70 <= 0
 c160: x22 + x26 - x69  = -2
 c161: x25 - x26 - x70  = -3
 c162: x22 + x26 - x27 - 3 x48 - 3 x49 <= -3
 c163: x25 - x26 - x30 - 34 x48 + 34 x49 <= 30
 c164: - x22 + x27 + x31 + 3 x48 - 3 x49 <= 0
 c165: - x25 + x30 - x31 + 34 x48 + 34 x49 <= 64
 c166: x22 + x26 - x32 - 3 x53 - 3 x54 <= -3
 c167: x25 - x26 - x35 - 34 x53 + 34 x54 <= 30
 c168: - x22 + x32 + 3 x53 - 3 x54 <= 1
 c169: - x25 + x35 + 34 x53 + 34 x54 <= 66
 c170: x22 + x26 - x37 - 3 x59 - 3 x60 <= -3
 c171: x25 - x26 - x40 - 34 x59 + 34 x60 <= 30
 c172: - x22 + x37 + 3 x59 - 3 x60 <= 1
 c173: - x25 + x40 + 34 x59 + 34 x60 <= 66
 c174: x22 + x26 - x42 - 3 x63 - 3 x64 <= -3
 c175: x25 - x26 - x45 - 34 x63 + 34 x64 <= 30
 c176: - x22 + x42 + x46 + 3 x63 - 3 x64 <= 2
 c177: - x25 + x45 - x46 + 34 x63 + 34 x64 <= 66
 c178: x22 + x26 - x47 - 3 x67 - 3 x68 <= -3
 c179: x25 - x26 - x50 - 34 x67 + 34 x68 <= 30
 c180: - x22 + x47 + x51 + 3 x67 - 3 x68 <= 2
 c181: - x25 + x50 - x51 + 34 x67 + 34 x68 <= 66
 c182: x22 + x26 - x52 - 3 x71 - 3 x72 <= -3
 c183: x25 - x26 - x55 - 34 x71 + 34 x72 <= 30
 c184: - x22 + x52 + x56 + 3 x71 - 3 x72 <= 2
 c185: - x25 + x55 - x56 + 34 x71 + 34 x72 <= 66
 c186: x3 - x30 + x31 >= 4
 c187: x27 - x73 <= 0
 c188: x30 - x74 <= 0
 c189: x27 + x31 - x73  = -2
 c190: x30 - x31 - x74  = -3
 c191: x27 + x31 - x32 - 3 x59 - 3 x60 <= -3
 c192: x30 - x31 - x35 - 34 x59 + 34 x60 <= 30
 c193: - x27 + x32 + 3 x59 - 3 x60 <= 1
 c194: - x30 + x35 + 34 x59 + 34 x60 <= 66
 c195: x27 + x31 - x37 - 3 x63 - 3 x64 <= -3
 c196: x30 - x31 - x40 - 34 x63 + 34 x64 <= 30
 c197: - x27 + x37 + 3 x63 - 3 x64 <= 1
 c198: - x30 + x40 + 34 x63 + 34 x64 <= 66
 c199: x27 + x31 - x42 - 3 x67 - 3 x68 <= -3
 c200: x30 - x31 - x45 - 34 x67 + 34 x68 <= 30
 c201: - x27 + x42 + x46 + 3 x67 - 3 x68 <= 2
 c202: - x30 + x45 - x46 + 34 x67 + 34 x68 <= 66
 c203: x27 + x31 - x47 - 3 x71 - 3 x72 <= -3
 c204: x30 - x31 - x50 - 34 x71 + 34 x72 <= 30
 c205: - x27 + x47 + x51 + 3 x71 - 3 x72 <= 2
 c206: - x30 + x50 - x51 + 34 x71 + 34 x72 <= 66
 c207: x27 + x31 - x52 - 3 x75 - 3 x76 <= -3
 c208: x30 - x31 - x55 - 34 x75 + 34 x76 <= 30
 c209: - x27 + x52 + x56 + 3 x75 - 3 x76 <= 2
 c210: - x30 + x55 - x56 + 34 x75 + 34 x76 <= 66
 c211: x3 - x35 >= 2
 c212: x32 - x77 <= 0
 c213: x35 - x78 <= 0
 c214: x32 - x77  = -1
 c215: x35 - x78  = -1
 c216: x32 - x37 - 3 x67 - 3 x68 <= -2
 c217: x35 - x40 - 34 x67 + 34 x68 <= 32
 c218: - x32 + x37 + 3 x67 - 3 x68 <= 1
 c219: - x35 + x40 + 34 x67 + 34 x68 <= 66
 c220: x32 - x42 - 3 x71 - 3 x72 <= -2
 c221: x35 - x45 - 34 x71 + 34 x72 <= 32
 c222: - x32 + x42 + x46 + 3 x71 - 3 x72 <= 2
 c223: - x35 + x45 - x46 + 34 x71 + 34 x72 <= 66
 c224: x32 - x47 - 3 x75 - 3 x76 <= -2
 c225: x35 - x50 - 34 x75 + 34 x76 <= 32
 c226: - x32 + x47 + x51 + 3 x75 - 3 x76 <= 2
 c227: - x35 + x50 - x51 + 34 x75 + 34 x76 <= 66
 c228: x32 - x52 - 3 x79 - 3 x80 <= -2
 c229: x35 - x55 - 34 x79 + 34 x80 <= 32
 c230: - x32 + x52 + x56 + 3 x79 - 3 x80 <= 2
 c231: - x35 + x55 - x56 + 34 x79 + 34 x80 <= 66
 c232: x3 - x40 >= 2
 c233: x37 - x81 <= 0
 c234: x40 - x82 <= 0
 c235: x37 - x81  = -1
 c236: x40 - x82  = -1
 c237: x37 - x42 - 3 x75 - 3 x76 <= -2
 c238: x40 - x45 - 34 x75 + 34 x76 <= 32
 c239: - x37 + x42 + x46 + 3 x75 - 3 x76 <= 2
 c240: - x40 + x45 - x46 + 34 x75 + 34 x76 <= 66
 c241: x37 - x47 - 3 x79 - 3 x80 <= -2
 c242: x40 - x50 - 34 x79 + 34 x80 <= 32
 c243: - x37 + x47 + x51 + 3 x79 - 3 x80 <= 2
 c244: - x40 + x50 - x51 + 34 x79 + 34 x80 <= 66
 c245: x37 - x52 - 3 x83 - 3 x84 <= -2
 c246: x40 - x55 - 34 x83 + 34 x84 <= 32
 c247: - x37 + x52 + x56 + 3 x83 - 3 x84 <= 2
 c248: - x40 + x55 - x56 + 34 x83 + 34 x84 <= 66
 c249: x3 - x45 + x46 >= 2
 c250: x42 - x85 <= 0
 c251: x45 - x86 <= 0
 c252: x42 + x46 - x85  = 0
 c253: x45 - x46 - x86  = -1
 c254: x42 + x46 - x47 - 3 x83 - 3 x84 <= -1
 c255: x45 - x46 - x50 - 34 x83 + 34 x84 <= 32
 c256: - x42 + x47 + x51 + 3 x83 - 3 x84 <= 2
 c257: - x45 + x50 - x51 + 34 x83 + 34 x84 <= 66
 c258: x42 + x46 - x52 - 3 x87 - 3 x88 <= -1
 c259: x45 - x46 - x55 - 34 x87 + 34 x88 <= 32
 c260: - x42 + x52 + x56 + 3 x87 - 3 x88 <= 2
 c261: - x45 + x55 - x56 + 34 x87 + 34 x88 <= 66
 c262: x3 - x50 + x51 >= 2
 c263: x47 - x89 <= 0
 c264: x50 - x90 <= 0
 c265: x47 + x51 - x89  = 0
 c266: x50 - x51 - x90  = -1
 c267: x47 + x51 - x52 - 3 x91 - 3 x92 <= -1
 c268: x50 - x51 - x55 - 34 x91 + 34 x92 <= 32
 c269: - x47 + x52 + x56 + 3 x91 - 3 x92 <= 2
 c270: - x50 + x55 - x56 + 34 x91 + 34 x92 <= 66
 c271: x3 - x55 + x56 >= 2
 c272: x52 - x93 <= 0
 c273: x55 - x94 <= 0
 c274: x52 + x56 - x93  = 0
 c275: x55 - x56 - x94  = -1
Bounds
      x1 = 0
 0 <= x2 <= 1
 1 <= x3 <= 34
      x4 = 0
 0 <= x5 <= 2
 0 <= x6 <= 33
 0 <= x7 <= 2
 0 <= x8 <= 1
 0 <= x9 <= 1
 0 <= x10 <= 33
 0 <= x11 <= 1
 0 <= x12 <= 2
 0 <= x13 <= 1
 0 <= x14 <= 1
 0 <= x15 <= 33
 0 <= x16 <= 1
 0 <= x17 <= 2
 0 <= x18 <= 1
 0 <= x19 <= 1
 0 <= x20 <= 33
 0 <= x21 <= 1
 0 <= x22 <= 2
 0 <= x23 <= 1
 0 <= x24 <= 1
 0 <= x25 <= 33
 0 <= x26 <= 1
 0 <= x27 <= 2
 0 <= x28 <= 1
 0 <= x29 <= 1
 0 <= x30 <= 33
 0 <= x31 <= 1
 0 <= x32 <= 2
 0 <= x33 <= 1
 0 <= x34 <= 1
 0 <= x35 <= 33
 0 <= x36 <= 1
 0 <= x37 <= 2
 0 <= x38 <= 1
 0 <= x39 <= 1
 0 <= x40 <= 33
 0 <= x41 <= 1
 0 <= x42 <= 2
 0 <= x43 <= 1
 0 <= x44 <= 1
 0 <= x45 <= 33
 0 <= x46 <= 1
 0 <= x47 <= 2
 0 <= x48 <= 1
 0 <= x49 <= 1
 0 <= x50 <= 33
 0 <= x51 <= 1
 0 <= x52 <= 2
 0 <= x53 <= 1
 0 <= x54 <= 1
 0 <= x55 <= 33
 0 <= x56 <= 1
 0 <= x57 <= 2
 0 <= x58 <= 33
 0 <= x59 <= 1
 0 <= x60 <= 1
 0 <= x61 <= 2
 0 <= x62 <= 33
 0 <= x63 <= 1
 0 <= x64 <= 1
 0 <= x65 <= 2
 0 <= x66 <= 33
 0 <= x67 <= 1
 0 <= x68 <= 1
 0 <= x69 <= 2
 0 <= x70 <= 33
 0 <= x71 <= 1
 0 <= x72 <= 1
 0 <= x73 <= 2
 0 <= x74 <= 33
 0 <= x75 <= 1
 0 <= x76 <= 1
 0 <= x77 <= 2
 0 <= x78 <= 33
 0 <= x79 <= 1
 0 <= x80 <= 1
 0 <= x81 <= 2
 0 <= x82 <= 33
 0 <= x83 <= 1
 0 <= x84 <= 1
 0 <= x85 <= 2
 0 <= x86 <= 33
 0 <= x87 <= 1
 0 <= x88 <= 1
 0 <= x89 <= 2
 0 <= x90 <= 33
 0 <= x91 <= 1
 0 <= x92 <= 1
 0 <= x93 <= 2
 0 <= x94 <= 33
Binaries
 x2  x8  x9  x11  x13  x14  x16  x18  x19  x21  x23  x24  x26  x28  x29  x31 
 x33  x34  x36  x38  x39  x41  x43  x44  x46  x48  x49  x51  x53  x54  x56 
 x59  x60  x63  x64  x67  x68  x71  x72  x75  x76  x79  x80  x83  x84  x87 
 x88  x91  x92 
Generals
 x1  x3  x4  x5  x6  x7  x10  x12  x15  x17  x20  x22  x25  x27  x30  x32  x35 
 x37  x40  x42  x45  x47  x50  x52  x55  x57  x58  x61  x62  x65  x66  x69 
 x70  x73  x74  x77  x78  x81  x82  x85  x86  x89  x90  x93  x94 
End
