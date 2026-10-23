package p

trait Tag[T]

object M {
  val v = Macros.subs[Shape]
}

// Completing S types `M.v.type`, the macro calls `knownDirectSubclasses` of Shape, which completes the
// permitted subclasses. That must not report a cyclic reference, but rather the same error as for Scala.
final class S extends Shape with Tag[M.v.type]
