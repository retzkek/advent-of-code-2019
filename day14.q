/ Split a string into a list of substrings
/ e.g. "R8,U5,L5,D3" => ("R8"; "U5"; "L5"; "D3")
split:{[s;delim] -1 {(z+1;y-z+1) sublist x}[s]': ss[s;delim],count s}

/ extract reactants as dictionary, e.g
/ "7 A, 1 B" => "AB"!7 1
reactants:{rs:parse each trim split[x;","];
 (last each rs)!(first each rs)}
/ parse reaction into dictionary, with reactants negative and products
/ positive amounts. e.g.
/ "7 A, 1 E => 1 FUEL" => `A`E`FUEL!-7 -1 1
reaction:{s:ss[x;"=>"];r:((0,s) sublist x);
 p:(((s+2),(count x)) sublist x);
 (neg reactants[r])^reactants[p]}

/ compile table of reactions e.g.
/ ORE A  B  C  D  E  FUEL
/ -----------------------
/ -10 10 0  0  0  0  0
/ -1  0  1  0  0  0  0
/ 0   -7 -1 1  0  0  0
/ 0   -7 0  -1 1  0  0
/ 0   -7 0  0  -1 1  0
/ 0   -7 0  0  0  -1 1
rx:{[rs]k:(key (+/) rs),`X;{(x!(count x)#0)^y}[k] each rs}


t:rx reaction each read0 `:day14.test1.txt
t:update X:1 from t where FUEL>0
