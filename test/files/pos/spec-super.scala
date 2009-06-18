import scala.collection.immutable._
import scala.collection.mutable.ListBuffer
import scala.collection.generic._

trait Base[+A] extends Traversable[A] {
  def add[B >: A, That](that: Traversable[B])(implicit bf: BuilderFactory[B, That, Base[A]]): That = {
    val b = bf(this)
    b ++= this
    b ++= that
    b.result
  }

}

abstract class Derived[@specialized +A] extends Base[A] {
  override def add[B >: A, That](that: Traversable[B])(implicit bf: BuilderFactory[B, That, Base[A]]): That = {
    val b = bf(this)
    super.add[B, That](that)
  }
}
