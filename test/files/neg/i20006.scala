
abstract class XIterateIterator[T](seed: T) extends collection.AbstractIterator[T] {
  private var first = true
  private var acc = seed
  val hasNext: T => Boolean
  val next: T => T
  override def hasNext: Boolean = first || hasNext(acc) // error
  override def next(): T = { // error
    if (first) {
      first = false
    } else {
      acc = next(acc)
    }
    acc
  }
}

final class YIterateIterator[T](seed: T, hasNext: T => Boolean, next: T => T) extends collection.AbstractIterator[T] {
  private var first = true
  private var acc = seed
  override def hasNext: Boolean = first || hasNext(acc) // error
  override def next(): T = { // error
    if (first) {
      first = false
    } else {
      acc = next(acc)
    }
    acc
  }
}

final class ZIterateIterator[T](seed: T, hasNext: T => Boolean, next: T => T) {
  private var first = true
  private var acc = seed
  def hasNext: Boolean = first || hasNext(acc) // error
  def next(): T = { // error
    if (first) {
      first = false
    } else {
      acc = next(acc)
    }
    acc
  }
}

class C(x: String) {
  val x: String = "member" // error
}
class D(x: String) {
  private var x: Int = 42 // error
}
class E(x: String) {
  private[this] var x: Int = 42 // error
}
class F(x: String) {
  def x(): Int = 42 // error
}
class G(x: String) {
  def x(i: Int): Int = i
}
class H {
  private[this] val x: String = ""
  def x(): Int = 42 // error
}
class I {
  private[this] def x: String = ""
  def x(): Int = 42 // error
}

/*
-- [E120] Naming Error: test/files/neg/i20006.scala:8:15 ---------------------------------------------------------------
8 |  override def hasNext: Boolean = first || hasNext(acc)
  |               ^
  |               Double definition:
  |               val hasNext: T => Boolean in class XIterateIterator at line 6 and
  |               override def hasNext: Boolean in class XIterateIterator at line 8
-- [E120] Naming Error: test/files/neg/i20006.scala:9:15 ---------------------------------------------------------------
9 |  override def next(): T = {
  |               ^
  |               Double definition:
  |               val next: T => T in class XIterateIterator at line 7 and
  |               override def next(): T in class XIterateIterator at line 9
-- [E120] Naming Error: test/files/neg/i20006.scala:22:15 --------------------------------------------------------------
22 |  override def hasNext: Boolean = first || hasNext(acc)
   |               ^
   |               Double definition:
   |               private[this] val hasNext: T => Boolean in class YIterateIterator at line 19 and
   |               override def hasNext: Boolean in class YIterateIterator at line 22
-- [E120] Naming Error: test/files/neg/i20006.scala:23:15 --------------------------------------------------------------
23 |  override def next(): T = {
   |               ^
   |               Double definition:
   |               private[this] val next: T => T in class YIterateIterator at line 19 and
   |               override def next(): T in class YIterateIterator at line 23
-- [E120] Naming Error: test/files/neg/i20006.scala:36:6 ---------------------------------------------------------------
36 |  def hasNext: Boolean = first || hasNext(acc)
   |      ^
   |      Double definition:
   |      private[this] val hasNext: T => Boolean in class ZIterateIterator at line 33 and
   |      def hasNext: Boolean in class ZIterateIterator at line 36
-- [E120] Naming Error: test/files/neg/i20006.scala:37:6 ---------------------------------------------------------------
37 |  def next(): T = {
   |      ^
   |      Double definition:
   |      private[this] val next: T => T in class ZIterateIterator at line 33 and
   |      def next(): T in class ZIterateIterator at line 37
-- [E120] Naming Error: test/files/neg/i20006.scala:48:6 ---------------------------------------------------------------
48 |  val x: String = "member" // error
   |      ^
   |      Double definition:
   |      private[this] val x: String in class C at line 47 and
   |      val x: String in class C at line 48
-- [E120] Naming Error: test/files/neg/i20006.scala:51:14 --------------------------------------------------------------
51 |  private var x: Int = 42 // error
   |              ^
   |              Double definition:
   |              private[this] val x: String in class D at line 50 and
   |              private[this] var x: Int in class D at line 51
-- [E120] Naming Error: test/files/neg/i20006.scala:54:20 --------------------------------------------------------------
54 |  private[this] var x: Int = 42 // error
   |                    ^
   |                    Double definition:
   |                    private[this] val x: String in class E at line 53 and
   |                    private[this] var x: Int in class E at line 54
-- [E120] Naming Error: test/files/neg/i20006.scala:57:6 ---------------------------------------------------------------
57 |  def x(): Int = 42 // error
   |      ^
   |      Double definition:
   |      private[this] val x: String in class F at line 56 and
   |      def x(): Int in class F at line 57
-- [E120] Naming Error: test/files/neg/i20006.scala:65:6 ---------------------------------------------------------------
65 |  def x(): Int = 42
   |      ^
   |      Double definition:
   |      val x: String in class H at line 63 and
   |      def x(): Int in class H at line 65
-- Warning: test/files/neg/i20006.scala:54:16 --------------------------------------------------------------------------
54 |  private[this] var x: Int = 42 // error
   |                ^
   |                Ignoring [this] qualifier.
   |                This syntax will be deprecated in the future; it should be dropped.
   |                See: https://docs.scala-lang.org/scala3/reference/dropped-features/this-qualifier.html
   |                This construct can be rewritten automatically under -rewrite -source 3.4-migration.
1 warning found
11 errors found
*/
