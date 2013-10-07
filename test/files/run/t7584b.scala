object Test extends App {
  def fold[A, B](f: (A, => B) => B) = (b: B) => f(null.asInstanceOf[A], b)
  def f[A, B](x: A, y: B): B = y
  def bip[A, B] = fold[A, B]((x, y) => f(x, y))
  def bop[A, B] = fold[A, B](f(_, _))

  // these work:
  fold[Int, Int]((x, y) => f(x, y))(0)
  fold[Int, Int](f(_, _))(0)

  // Used to throw a ClassCastException. Since the fix for SI-7899, these issue type errors.
  // fold[Int, Int](f _)(0)
  // fold[Int, Int](f)(0)
}
