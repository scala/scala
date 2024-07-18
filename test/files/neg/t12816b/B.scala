//> using options -Xsource:3 -Werror

package p {
  object c
  trait Z
  trait T {
    def a = 1
    def b = 1
    def c = 1

    trait X
    trait Y
    trait Z
  }

  trait RR extends T {
    def m1 = a // ok
    def m2 = b // ok
    def m3 = c // warn

    def n1: X // ok
    def n2: Y // ok
    def n3: Z // warn
  }
}
