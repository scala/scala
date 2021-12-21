//scalac: -Yannotations:scala.annotation
package p

// use nowarn without prefix
@nowarn
class C

import collection.AbstractIterator

abstract class ConcatIterator[+A](var current: Iterator[A @unchecked.uncheckedVariance]) extends AbstractIterator[A]

// was: error: not found: type nowarn
