import test.C

object Test {
  def main(args: Array[String]) {
    val c = new C
    val d = new c.D

    // This inlining fails with separate compilation.
    //
    // [log inliner]    class  Test                          // analyzing 2 methods in test.Test
    // [log inliner] ICodeReader reading class C$D
    // [log inliner]        0  D.skates                      // don't inline into Test.main
    // [log inliner]   access  instruction LOAD_FIELD value $outer requires private access // pos=NoPosition
    // [log inliner]     fail  D.skates                      // access level required by callee not matched by caller
    //
    // ExplicitOuter doesn't publicise C$D#outer$; it only transforms:
    //       @inline def skates(a: Any): Unit = C.this.x_=(a.toString())
    // into
    //       @inline def skates(a: Any): Unit = D.this.$outer.x_=(a.toString());
    // but after processing the This node it doesn't go further to process the
    // resultant Select() node to discover that $outer is accessed from within
    // an @inline-d method.
    //
    // Changing ExplicitOuter to do this, we hit another problem: makeNotPrivate renames the local name
    // "outer$ ".
    d.skates("x")
    assert(c.x == "x")
  }
}
