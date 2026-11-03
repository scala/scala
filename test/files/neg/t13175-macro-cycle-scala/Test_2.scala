package p

trait Tag[T]

object M {
  val v = Macros.subs[Shape]
}

sealed trait Shape
final class S extends Shape with Tag[M.v.type]
