/ run unary fn over the list of tests, where each test is a list of
/ the input and expected output
run_tests:{[fn; tests] (&/) {
  -2"f[",string[y[0]],"]=",string[r:x[y[0]]]," ? ",string[y[1]];
  ?[y[1]=r;"pass";"fail"]
  }[fn] each tests}

fuel:{(floor x%3)-2}
-1"fuel:",run_tests[fuel; (12 2;14 2;1969 654;100756 33583)];

/ fuel2 computes fuel required by mass x including the mass of the fuel itself
fuel2:{sum fuel\[{6 < x}; fuel x]}
-1"fuel2:",run_tests[fuel2; (1969 966; 100756 50346)];

p:read0 `:day1.input.txt
/ convert to longs
m:{"J"$x} each p
show sum fuel each m
show sum fuel2 each m

exit 0
