trait ToString { def convert(x: Int): String }

class ExplicitSamType {
  object O {
    def m(x: Int => String): Int = 0 // (1)
    def m(x: ToString): Int = 1      // (2)
  }

  O.m((x: Int) => x.toString) // ok, function type takes precedence, because (1) is more specific than (2),
  // because (1) is as specific as (2): (2) can be applied to a value of type Int => String (well, assuming it's a function literal)
  // but (2) is not as specific as (1): (1) cannot be applied to a value of type ToString

  O.m(_.toString) // ok: overloading resolution pushes through `Int` as the argument type, so this type checks
  O.m(x => x) // error expected: m cannot be applied to Int => Int
}
