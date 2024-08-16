//> using options -Werror -Xsource:3 -Xsource-features:double-definitions

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
class PrivateConflict {
  private[this] var x = 42
  def x(): Int = x // error
  def x_=(n: Int) = x = n
}
class LocalConflict {
  def f(): Unit = {
    var x = 42
    def x(): Int = x // error
  }
}
