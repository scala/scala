package scala.collection
package immutable

import mutable.{Builder, StringBuilder}

/**
  *  This class serves as a wrapper augmenting `String`s with all the operations
  *  found in indexed sequences.
  *
  *  The difference between this class and `StringOps` is that calling transformer
  *  methods such as `filter` and `map` will yield an object of type `WrappedString`
  *  rather than a `String`.
  *
  *  @param self    a string contained within this wrapped string
  *
  *  @since 2.8
  *  @define Coll `WrappedString`
  *  @define coll wrapped string
  */
final class WrappedString(private val self: String) extends AbstractSeq[Char] with IndexedSeq[Char]
  with IndexedSeqOps[Char, IndexedSeq, WrappedString] {

  def apply(i: Int): Char = self.charAt(i)

  override protected def fromSpecific(coll: scala.collection.IterableOnce[Char]): WrappedString =
    WrappedString.fromSpecific(coll)
  override protected def newSpecificBuilder: Builder[Char, WrappedString] = WrappedString.newBuilder

  override def slice(from: Int, until: Int): WrappedString = {
    val start = if (from < 0) 0 else from
    if (until <= start || start >= self.length)
      return WrappedString.empty

    val end = if (until > length) length else until
    new WrappedString(self.substring(start, end))
  }
  override def length = self.length
  override def toString = self
  override def view: StringView = new StringView(self)

  override protected[this] def className = "WrappedString"

  override def equals(other: Any): Boolean = other match {
    case that: WrappedString =>
      this.self == that.self
    case _ =>
      super.equals(other)
  }
}

/** A companion object for wrapped strings.
  *
  *  @since 2.8
  */
@SerialVersionUID(3L)
object WrappedString extends SpecificIterableFactory[Char, WrappedString] {
  def fromSpecific(it: IterableOnce[Char]): WrappedString = {
    val b = newBuilder
    val s = it.knownSize
    if(s >= 0) b.sizeHint(s)
    b ++= it
    b.result()
  }
  val empty: WrappedString = new WrappedString("")
  def newBuilder: Builder[Char, WrappedString] =
    new StringBuilder().mapResult(x => new WrappedString(x))

  implicit class UnwrapOp(private val value: WrappedString) extends AnyVal {
    def unwrap: String = value.self
  }
}
