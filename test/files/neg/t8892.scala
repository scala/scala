trait A { type B = String }
class C[B](x: B) extends A { def f: B = x }
