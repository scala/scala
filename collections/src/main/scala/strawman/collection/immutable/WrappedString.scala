package strawman.collection
package immutable

import scala.{Char, Int}
import scala.Predef.String
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
final class WrappedString(val self: String) extends AbstractSeq[Char] with IndexedSeq[Char] {

  def apply(i: Int): Char = self.charAt(i)

  protected[this] def fromSpecificIterable(coll: strawman.collection.Iterable[Char]): IndexedSeq[Char] =
    WrappedString.fromSpecific(coll)
  protected[this] def newSpecificBuilder(): Builder[Char, IndexedSeq[Char]] = WrappedString.newBuilder()
  def iterableFactory: SeqFactory[IndexedSeq] = ImmutableArray

  override def slice(from: Int, until: Int): WrappedString = {
    val start = if (from < 0) 0 else from
    if (until <= start || start >= self.length)
      return new WrappedString("")

    val end = if (until > length) length else until
    new WrappedString(self.substring(start, end))
  }
  override def length = self.length
  override def toString = self
  override def view: StringView = new StringView(self)
}

/** A companion object for wrapped strings.
  *
  *  @since 2.8
  */
object WrappedString extends SpecificIterableFactory[Char, WrappedString] {
  def fromSpecific(it: IterableOnce[Char]): WrappedString = {
    val b = newBuilder()
    val s = it.knownSize
    if(s >= 0) b.sizeHint(s)
    b ++= it
    b.result()
  }
  val empty: WrappedString = new WrappedString("")
  def newBuilder(): Builder[Char, WrappedString] =
    new StringBuilder().mapResult(x => new WrappedString(x))
}
