abstract class ImplicitRepeated {
  trait T[+A, +B]
  trait X

  def f[N, R <: List[_]](elems: T[N, R]*)                // alternative a)
  def f[N, R <: List[_]](props: String, elems: T[N, R]*) // alternative b)

  // the following implicit causes "cannot be applied" errors
  implicit def xToRight(r: X): T[Nothing, X] = null
  implicit def anyToN[N](x: N): T[N, Nothing] = null


  f("A", 1, 2) // should be implicitly resolved to alternative b)
  f( 1, 2 )    // should be implicitly resolved to alternative a)
    // ImplicitRepeated.this.f[Int, Nothing]("A", ImplicitRepeated.this.anyToN[Int](1), ImplicitRepeated.this.anyToN[Int](2));
    // ImplicitRepeated.this.f[Int, Nothing](ImplicitRepeated.this.anyToN[Int](1), ImplicitRepeated.this.anyToN[Int](2))  
}