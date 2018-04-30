// Unicode newline in a single-line comment?
// Compiler will expect code on the line.
// Here the code is invalid on line 11.

/* \n foo */
// \n foo
/* \12 foo */
// \12 foo
/* \u000a foo */
// foo \u000a
// \u000a foo
