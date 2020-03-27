// scalac: -Werror -Xsource:3

class Test {
  val b1 = {
    22
    * 22  // ok
    */*one more*/22 // error: end of statement expected
  }                 // error: ';' expected, but '}' found
}
