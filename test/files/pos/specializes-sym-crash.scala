import scala.collection._

trait IterableViewLike[+A,
+Coll,
+This <: IterableView[A, Coll] with IterableViewLike[A, Coll, This]] {
  def viewToString: String = ""
  protected[this] def viewIdentifier: String = ""
  trait Transformed[+B]
}
trait IterableView[+A, +Coll] extends IterableViewLike[A, Coll, IterableView[A, Coll]]
trait SeqView[+A, +Coll] extends SeqViewLike[A, Coll, SeqView[A, Coll]]
trait SeqViewLike[+A,
+Coll,
+This <: SeqView[A, Coll] with SeqViewLike[A, Coll, This]]
  extends Seq[A] with SeqOps[A, Seq, Seq[A]] with IterableView[A, Coll] with IterableViewLike[A, Coll, This]


trait Foo[+A,
+Coll,
+This <: SeqView[A, Coll] with SeqViewLike[A, Coll, This]]
extends Seq[A] with SeqOps[A, Seq, Seq[A]] with IterableView[A, Coll] with IterableViewLike[A, Coll, This] {
self =>

  trait Transformed[+B] extends SeqView[B, Coll] with super.Transformed[B] {
    def length: Int
    def apply(idx: Int): B
    override def toString = viewToString
  }
  trait Reversed extends Transformed[A] {
    override def iterator: Iterator[A] = createReversedIterator
    def length: Int = self.length
    def apply(idx: Int): A = self.apply(length - 1 - idx)
    final override protected[this] def viewIdentifier = "R"

    private def createReversedIterator = {
      var lst = List[A]()
      for (elem <- self) lst ::= elem
      lst.iterator
    }
  }
}
