//> using options -Xlint:infer-any -Werror

// https://github.com/scala/bug/issues/13128
trait Bippy {
  def f[F[_]](i: Int)(ev: F[Int]): Int
  def t = f(1)(Some(1)) // no warn

  def f1[F[_]](i: Int): Int
  def t1 = f1(1) // warn

  def f2[F[_]](i: Int)(ev: F[Int] => Int): Int
  def t2 = f2(1)((x: Some[Int]) => 1) // warn
}
