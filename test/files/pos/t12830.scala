//> using options -Xsource:3

class C {
  def i: Int = 42
}
object D extends C {
  override final val i = 27
}
object Test {
  def f: 27 = D.i
}

/*
t12830.scala:10: error: type mismatch;
 found   : D.i.type (with underlying type Int)
 required: 27
  def f: 27 = D.i
                ^
t12830.scala:7: error: under -Xsource:3, inferred Int instead of Int(27)
  override final val i = 27
                     ^
2 errors
*/
