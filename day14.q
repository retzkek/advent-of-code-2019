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

br:{r:?[x;();0b;(enlist `sum)!(enlist (sum;(*;y;`X)))];r[`sum][0]} / balance for reactant
inc:{![x;enlist (>;y;0);0b;(enlist `X)!(enlist (+;`X;1))]} / increase multiplier for reaction where reactant is product
ir:{(cols x) {x _ x?y}/ `ORE`FUEL`X} / intermediate reactants
nr:{ir[x] where (br[x] each ir[x])<0} / negative reactants
inr:{inc[x; first nr[x]]} /increase multiplier for a negative reactants
sx:{{0<count nr[x]}inr/x} / iterate multipliers until reactants are positive
solve:{
 t:rx reaction each read0 x; / read reaction into table
 t:update X:1 from t where FUEL>0; / require 1 FUEL
 t:sx t; / balance reactions
 neg first first select sum(ORE*X) from t} / return required ORE
/ tests
solve[`:day14.test1.txt]=31
solve[`:day14.test2.txt]=13312
solve[`:day14.test3.txt]=180697
solve[`:day14.test4.txt]=2210736
/ problem
solve[`:day14.input.txt]

exit 0
