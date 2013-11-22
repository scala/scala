object coinv {
  def up[F[+_]](fa: F[String]): F[Object] = fa
  def down[F[-_]](fa: F[Object]): F[String] = fa
 
  up(List("hi"))
  // [error] type A is covariant, but type _ is declared contravariant
  down(List('whatever: Object))
}
