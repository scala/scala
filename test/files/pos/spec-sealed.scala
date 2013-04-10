sealed abstract class MyList[@specialized +A] {
  def head: A
  def tail: MyList[A]

  def ::[@specialized B >: A](x: B): MyList[B] =
    new Cons[B](x, this)
}

case object MyNil extends MyList[Nothing] {
  def head = sys.error("nil")
  def tail = sys.error("nil")
}

case class Cons[@specialized a](private val hd: a, tl: MyList[a]) extends MyList[a] {
  def head = hd
  def tail = tl
}

abstract class IntList extends MyList[Int]

object Main extends App {
  val xs = 1 :: 2 :: 3 :: MyNil
  println(xs)
}

/*
final class ConsI(hd1: Int, tl1: MyList[Int]) extends Cons[Int](hd1, tl1) {
  override val hd = hd1
  override val tl = tl1
}
*/
//class IntCons(_hd: Int, _tl: MyList[Int]) extends Cons[Int](_hd, _tl)
