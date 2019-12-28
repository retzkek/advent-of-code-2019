.intcode.P:enlist[0]!enlist[99] / base program, just exits
.intcode.RB:0 / relative base address

/ Read an intcode program from comma-delimited string. The program is
/ stored in a dictiory with the address as the key since the program
/ can access arbitrary addresses.
.intcode.parse:{.intcode.P:(til count p)!p:parse ssr[x;",";" "];.intcode.P}
/ Calculate the parameter value based on the parameter mode
.intcode.val:{[x; mode] ((0;1;2)!(.intcode.P[x];x;.intcode.P[.intcode.RB+x]))[mode]}
/ Calculate the parameter address based on the parameter mode
.intcode.adr:{[x; mode] ((0;2)!(x;.intcode.RB+x))[mode]}
.intcode.op.add:{}
/.intcode.ops:({})
/.intcode.exe:{[prog addr] }
