// scalac: -Xsource:2.13 -Werror

class AnyEqualsTest {
  1L equals 1
  // ok, because it's between the same numeric types
  1 equals 1
  // ok
  1L equals "string"
  // ok
  1L.equals(())
  (1L: Any) equals 1
  (1L: AnyVal) equals 1
  (1L: AnyVal) equals (1: AnyVal)
  // ok
  "string" equals 1
  def foo[A](a: A) = a.equals(1)
  // ok
  def bar[A <: AnyRef](a: A) = a.equals(1)
}
