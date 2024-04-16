//> using options -opt:inline:** -Wopt
package instrumented

object MyPredef {
  @inline
  final def assert(assertion: Boolean, message: => Any): Unit = {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: " + message)
  }
}

class Foo(x: Int) {
  MyPredef.assert(x > 0, "not positive: " + x)
}
