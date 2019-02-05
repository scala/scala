import scala.collection.BuildFrom

trait Base[+A] extends Iterable[A] {
  def add[B >: A, That](that: Iterable[B])(implicit bf: BuildFrom[Base[A], B, That]): That = {
    val b = bf(this)
    b ++= this
    b ++= that
    b.result
  }

}

abstract class Derived[@specialized +A] extends Base[A] {
  override def add[B >: A, That](that: Iterable[B])(implicit bf: BuildFrom[Base[A], B, That]): That = {
    val b = bf(this)
    super.add[B, That](that)
  }
}
