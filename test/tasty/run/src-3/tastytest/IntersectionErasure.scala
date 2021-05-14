package tastytest

object IntersectionErasure {

  trait A
  trait B

  @FunctionalInterface
  abstract class IntersectionSAM {
    def apply(arg: B with A): B with A
  }

  final class IntersectionVC(val unwrapped: B with A) extends AnyVal {
    def matchesInternal(that: B with A): Boolean = that == unwrapped
    def matches(that: IntersectionVC): Boolean = this == that
  }

  final class IntersectionVCParametric[T <: B with A](val unwrapped: T) extends AnyVal {
    def matchesInternal(that: T): Boolean = that == unwrapped
    def matches(that: IntersectionVCParametric[T]): Boolean = this == that
  }

}
