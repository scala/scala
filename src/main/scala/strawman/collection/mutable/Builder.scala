package strawman.collection.mutable

import scala.{Boolean, Any, Char}
import java.lang.String
import strawman.collection.{IterableMonoTransforms, IterableOnce}

/** Base trait for collection builders */
trait Builder[-A, +To] { self =>

  /** Append an element */
  def +=(x: A): this.type

  /** Result collection consisting of all elements appended so far. */
  def result: To

  /** Bulk append. Can be overridden if specialized implementations are available. */
  def ++=(xs: IterableOnce[A]): this.type = {
    xs.iterator().foreach(+=)
    this
  }

  /** A builder resulting from this builder my mapping the result using `f`. */
  def mapResult[NewTo](f: To => NewTo) = new Builder[A, NewTo] {
    def +=(x: A): this.type = { self += x; this }
    override def ++=(xs: IterableOnce[A]): this.type = { self ++= xs; this }
    def result: NewTo = f(self.result)
  }
}

/** Base trait for strict collections that can be built using a builder.
  * @tparam  A    the element type of the collection
  * @tparam Repr  the type of the underlying collection
  */
trait Buildable[+A, +Repr] extends Any with IterableMonoTransforms[A, Repr]  {

  /** Creates a new builder. */
  protected[this] def newBuilder: Builder[A, Repr]

  /** Optimized, push-based version of `partition`. */
  override def partition(p: A => Boolean): (Repr, Repr) = {
    val l, r = newBuilder
    coll.iterator().foreach(x => (if (p(x)) l else r) += x)
    (l.result, r.result)
  }

  // one might also override other transforms here to avoid generating
  // iterators if it helps efficiency.
}

class StringBuilder extends Builder[Char, String] {
  private val sb = new java.lang.StringBuilder

  def += (x: Char) = { sb.append(x); this }

  /** Overloaded version of `++=` that takes a string */
  def ++= (s: String) = { sb.append(s); this }

  def result = sb.toString

  override def toString = result
}