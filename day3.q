/ Split a string into a list of substrings
/ e.g. "R8,U5,L5,D3" => ("R8"; "U5"; "L5"; "D3")
split:{[s;delim] -1 {(z+1;y-z+1) sublist x}[s]': ss[s;delim],count s}

/ Translations in the given direction
t:`U`D`R`L!(0 1; 0 -1; 1 0; -1 0)

/ Convert list of translations to list of line segments starting from 0 0.
/ e.g.  ("R8"; "U5"; "L5"; "D3") =>
/   0 0; 8 0
/   8 0; 8 5
/   8 5; 3 5
/   3 5; 3 2
seg:{[d] (0 0;0 0) {(x[1];x[1] + t[`$1#y] * "J"$1_y)}\ d}
/ include signal distance
segs:{[d] (0 0 0;0 0 0) {(x[1];x[1] + (t[`$1#y],1) * "J"$1_y)}\ d}

/ Determine if perpendicular line segments intersect
isectp:{(&/)(2#x[0]<y[0])<>(2#x[1]<y[1])}

/ Determine where perpendicular lines intersect
isectl:{(x[0]*x[0]=x[1])+(y[0]*y[0]=y[1])}

/ Determine total signal distance where perpendicular lines intersect
isects:{i:isectl[x;y];(x[0;2]+(max abs 2#(x[0]-i)))+(y[0;2]+(max abs 2#(y[0]-i)))}

/ Manhttan distance from origin
mdist:{(+/) abs x}

/ manhattan distance for each intersection between lists of line segments
xsegm:{x {?[isectp[x;y];mdist[isectl[x;y]];0N]}/:\: y}

/ signal distance for each intersection between lists of line segments
xsegs:{x {?[isectp[x;y];isects[x;y];0N]}/:\: y}

/ read problem
p:read0 `:day3.input.txt
/ split into translations then convert to line segments
s:{segs split[x;","]} each p
/ find minimum manhattan distance between each pair of line segments
show min min each xsegm[s[0];s[1]]
/ find minimum signal distance between each pair of line segments
show min min each xsegs[s[0];s[1]]

exit 0
