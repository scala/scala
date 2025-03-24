class ann(x: Int = 1, y: Int = T.i) extends annotation.StaticAnnotation
class kon(x: Int = 1, y: Int = 2) extends annotation.ConstantAnnotation

object T { def i = 0 }

class A {
  @ann(x = 11) def m1 = 1
  @ann(y = 22) def m2 = 1

  @kon(x = 11) def k1 = 1
  @kon(y = 22) def k2 = 1
}
