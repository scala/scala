// scalac: -deprecation
//
//
//
// scala pr #9972
object Test {
  import scala.collection.immutable.BitSet

  def main(args: Array[String]): Unit = {
    new LowerCaseString("Foo").fold(' ')((_, _) => ' ')
    Iterator.empty[Int].fold(0)(_ + _)

    List(1, 3, 5).fold(0)(_ + _)
    BitSet(1, 3, 5).fold(0)(_ + _)
    Map(1 -> 'a', 3 -> 'b', 5 -> 'c').fold(0 -> 'z')((_, _) => 10 -> 'x')

    Array(1, 3, 5).fold(0)(_ + _)
    "Foo".fold(' ')((_, _) => ' ')

    // Ensure Option.fold is not deprecated
    Option(10).fold(ifEmpty = 1)(x => (x * 3) + 5)
  }
}

final case class LowerCaseString(source: String) extends IterableOnce[Char] with scala.collection.IterableOnceOps[Char, Iterable, String] {
  override def iterator: Iterator[Char] = source.iterator.map(_.toLower)

  override def scanLeft[B](z: B)(op: (B, Char) => B): Iterable[B] = ???
  override def filter(p: Char => Boolean): String = ???
  override def filterNot(pred: Char => Boolean): String = ???
  override def take(n: Int): String = ???
  override def takeWhile(p: Char => Boolean): String = ???
  override def drop(n: Int): String = ???
  override def dropWhile(p: Char => Boolean): String = ???
  override def slice(from: Int, until: Int): String = ???
  override def map[B](f: Char => B): Iterable[B] = ???
  override def flatMap[B](f: Char => IterableOnce[B]): Iterable[B] = ???
  override def flatten[B](implicit asIterable: Char => IterableOnce[B]): Iterable[B] = ???
  override def collect[B](pf: PartialFunction[Char,B]): Iterable[B] = ???
  override def zipWithIndex: Iterable[(Char, Int)] = ???
  override def span(p: Char => Boolean): (String, String) = ???
  override def tapEach[U](f: Char => U): String = ???
}
