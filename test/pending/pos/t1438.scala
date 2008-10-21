class C[A] {
  type CC[B] <: C[B]
  def aio[T]: T = aio[T]
}
class D[A] extends C[A] {
  protected def nv[B](elems: Iterator[B]): CC[B] = {
    val x = new D[B]
    x.aio[CC[B]]
  }
}
