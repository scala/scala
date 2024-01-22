package tastytest

object IntersectionErasure {

  trait A
  trait B

  @FunctionalInterface
  abstract class IntersectionSAM {
    def apply(arg: B & A): B & A
  }

  final class IntersectionVC(val unwrapped: B & A) extends AnyVal {
    def matchesInternal(that: B & A): Boolean = that == unwrapped
    def matches(that: IntersectionVC): Boolean = this == that
  }

  final class IntersectionVCParametric[T <: B & A](val unwrapped: T) extends AnyVal {
    def matchesInternal(that: T): Boolean = that == unwrapped
    def matches(that: IntersectionVCParametric[T]): Boolean = this == that
  }

}
