// scalac: -Werror -Xsource:3

class Test {
  val x = 42
  val b2: Boolean = {
    println(x)
    ! "hello".isEmpty  // error: value ! is not a member of Unit
  }
}
