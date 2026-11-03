package p

trait Tag[T]

object M {
  val v = Macros.subs[Shape]
}

// Completing `S` runs the macro, which asks for the children of `Shape`, including `S`.
// This reports the same error as for a Scala hierarchy, not a cyclic reference.
final class S extends Shape with Tag[M.v.type]
