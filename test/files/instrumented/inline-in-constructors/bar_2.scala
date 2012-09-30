package instrumented

/** Class that uses assert compiled in previous compiler run so we check if
    inlining in constructors works across different compilation runs */
class Bar(x: Boolean) {
  MyPredef.assert(x, "not true: " + x)
}
