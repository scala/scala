package strawman.collection.mutable

import scala.{Boolean, Any, Char, Unit}
import java.lang.String
import strawman.collection.{IterableMonoTransforms, IterableOnce}

/** Base trait for collection builders */
trait Builder[-A, +To] extends Growable[A] { self =>

  /** Append an element */
  def +=(x: A): this.type

  /** Clears the contents of this builder.
   *  After execution of this method the builder will contain no elements.
   */
  def clear(): Unit

  /** Result collection consisting of all elements appended so far. */
  def result: To

  /** A builder resulting from this builder my mapping the result using `f`. */
  def mapResult[NewTo](f: To => NewTo) = new Builder[A, NewTo] {
    def +=(x: A): this.type = { self += x; this }
    def clear(): Unit = self.clear()
    override def ++=(xs: IterableOnce[A]): this.type = { self ++= xs; this }
    def result: NewTo = f(self.result)
  }
}

class StringBuilder extends Builder[Char, String] {
  private val sb = new java.lang.StringBuilder

  def += (x: Char) = { sb.append(x); this }

  def clear() = sb.setLength(0)

  /** Overloaded version of `++=` that takes a string */
  def ++= (s: String) = { sb.append(s); this }

  def result = sb.toString

  override def toString = result
}