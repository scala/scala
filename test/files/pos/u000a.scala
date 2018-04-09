// Unicode newline in a single-line comment?
// Compiler will expect code on the line.
// Here the code is valid.

/* \n object foo */
// \n object foo
/* \12 object foo */
// \12 object foo
/* \u000a object foo */
// object foo \u000a
// \u000a object foo
