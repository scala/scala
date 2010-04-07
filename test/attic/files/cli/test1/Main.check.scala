unknown option: '-cpp'
scala [ <option> ]... [<torun> <arguments>]

All options to scalac are allowed.  See scalac -help.

<torun>, if present, is an object or script file to run.
If no <torun> is present, run an interactive interpreter.

Option -howtorun allows explicitly specifying how to run <torun>:
    script: it is a script file
    object: it is an object name
    guess: (the default) try to guess

Option -savecompiled requests that the compiled script be saved
for future use.

Option -nocompdaemon requests that the fsc offline compiler not be used.

Option -Dproperty=value sets a Java system property.

1: test 1 passed (1)
1: test 2 passed (1)
1: test 3 passed (1)
1: test 4 passed (2)
