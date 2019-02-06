trait Traversable[+A] {
  def flatMap[B](f: A => Traversable[B]): Traversable[B] = ???
}

trait Iterable[+A] extends Traversable[A] {
  def flatMap[B](f: A => Iterable[B]): Iterable[B] = ???
}

trait Seq[+A] extends Iterable[A] {
  def flatMap[B](f: A => Seq[B]): Seq[B] = ???
}

object Test extends App {
  val a: Seq[Int] = new Seq[Int] {}
  val b: Iterable[Int] = new Iterable[Int] {}
  a.flatMap(i => b)
}
