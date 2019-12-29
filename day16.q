base:0 1 0 -1 / base pattern
pat:{raze {x#y}[x] each base} / single pattern for position x
patn:{x#(ceiling x%4*y) {y,pat[x]}[y]/1_pat[y]} / repeated pattern for signal of length x, position y
sig:{{"H"$x} each x} / convert signal to list
fft:{{mod[abs(+/)x*patn[count x; y];10]}[x] each 1+til count x}
f8:{8#100 fft/sig x}
/ tests
(&/)sig["24176176"]=f8 "80871224585914546619083218645595"
(&/)sig["73745418"]=f8 "19617804207202209144916044189917"
(&/)sig["52432133"]=f8 "69317163492948606335995924319873"
/problem
show raze string f8 first read0 `:day16.input.txt

exit 0
