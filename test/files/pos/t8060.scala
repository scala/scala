trait M[F[_]]
 
trait P[A] {
  type CC[X] = P[X]
  def f(p: A => Boolean): M[CC]
}
 
trait Other {
  // was infinite loop trying to dealias `x$1.CC`
  def g[A](p: A => Boolean): P[A] => M[P] = _ f p
}
