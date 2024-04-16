//> using options -Xsource:3
//

open class A
infix class B[T, S]

open infix class C[T, S]
open infix case class CC[T, S](x: Int)
infix open class D[T, S]
infix trait DT[T, S]

open
infix
private
class E

class F {
  open infix class C1[T, S]
  infix type X

  infix def foo(x: Int): Int = x
}

object G {
  open infix class C2[T, S]
}

object Test {
  val infix: Int = 1
  infix + 1
  val open: Int => Int = x => x
  open(1)
  open {
    2
  }
}
