// scalac: -Werror -Xsource:3

class Test {
  val x = 1
    + 2
    +3 // error: Expected a toplevel definition
}
