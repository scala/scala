trait ToString { def convert(x: Int): String }

class ExplicitSamType {
  object O {
    def m(x: Int => String): Int = 0
    def m(x: ToString): Int = 1
  }

  O.m((x: Int) => x.toString) // ok, function type takes precedence

  O.m(_.toString) // error expected: eta-conversion breaks down due to overloading
  O.m(x => x) // error expected: needs param type
}
