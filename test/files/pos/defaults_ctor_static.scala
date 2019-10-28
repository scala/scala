class C[TTT] {
  class K(f: TTT => Int = (x: TTT) => 1)
}

object Test {
  val c = new C[Int]
  new c.K()
}
/* Test compiles to
    public static {
        new C.K(MODULE$.c(), C.K.$lessinit$greater$default$1());
    }
*/
