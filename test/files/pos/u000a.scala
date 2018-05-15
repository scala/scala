// Unicode newline in a single-line comment?
// Compiler won't expect code on the line.

/* \n object foo */
// \n object foo
/* \12 object foo */
// \12 object foo
/* \u000a object foo */
// object foo \u000a
// \u000a object foo
/* \n foo */
// \n foo
/* \12 foo */
// \12 foo
/* \u000a foo */
// foo \u000a
// \u000a foo
