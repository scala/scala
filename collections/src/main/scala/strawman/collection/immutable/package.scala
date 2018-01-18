package strawman.collection

import scala.deprecated

package object immutable {
  type StringOps = strawman.collection.StringOps
  val StringOps = strawman.collection.StringOps
  type StringView = strawman.collection.StringView
  val StringView = strawman.collection.StringView

  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  type Traversable[+X] = Iterable[X]
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  val Traversable = Iterable
}
