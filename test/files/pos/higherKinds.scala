//> using options -feature -Werror
object Test {
  type F[A] <: List[A]
  def foo[G[X] <: List[X]](x: F[Int]): F[Int] = x
}
