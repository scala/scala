package strawman.collection

import scala.{deprecated, Any}

// We need this in scala-library because scala.reflect.internal.Types.shorthands forces initialization and
// sequenceToArray in UnCurry uses Traversable's type parameter to determine a collection's element type
@deprecated("Use Iterable instead of Traversable", "2.13.0")
trait Traversable[+A] extends Any
