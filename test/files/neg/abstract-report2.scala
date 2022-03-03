import java.util.Collection

class Foo extends Collection[Int]

class Bar extends Collection[List[_ <: String]]

class Baz[T] extends Collection[T]

trait Xyz[T] {
  def foo(x: T): Boolean
}

trait Symbolic {
  def --? : Int
  def --!(i: Int): Unit
  def unary_~ : Long
}

trait Bippy[T1, T2, T3] extends collection.IterableOps[(T2, String), List, List[(T2, String)]] with Xyz[T3]

class Dingus extends Bippy[String, Set[Int], List[Int]]

class JustOne extends Collection[Int] {
  def add(x$1: Int): Boolean = ???
  def addAll(x$1: java.util.Collection[_ <: Int]): Boolean = ???
  def clear(): Unit = ???
  def contains(x$1: Object): Boolean = ???
  def containsAll(x$1: java.util.Collection[_]): Boolean = ???
  def isEmpty(): Boolean = ???
  def iterator(): java.util.Iterator[Int] = ???
  def remove(x$1: Object): Boolean = ???
  def removeAll(x$1: java.util.Collection[_]): Boolean = ???
  def retainAll(x$1: java.util.Collection[_]): Boolean = ???
  def size(): Int = ???
  //def toArray[T](x$1: Array[T with Object]): Array[T with Object] = ???
  def toArray(): Array[Object] = ???
}
/* was:
test/files/neg/abstract-report2.scala:23: error: class JustOne needs to be abstract. Missing implementation for:
  def toArray[T](x$1: Array[T with Object]): Array[T with Object] // inherited from trait Collection
(Note that Array[T with Object] does not match java.util.function.IntFunction[Array[T with Object]])
class JustOne extends Collection[Int] {
 */

trait U { def t(): Int }
trait T extends U { abstract override def t(): Int = super.t() + 1 }
class C extends T               // refchecks
trait V extends U { abstract override def t(): Int = super.t() + 1 }
trait W extends V { abstract override def t(): Int = super.t() + 1 }
class D extends W
