trait T0 { def ap(): Int }
trait T1 { def ap(a: Any): Int }
trait T2 { def ap(a: Any, b: Any): Int }

class Test {
  (() => 0): T1
  ((x: Any) => 0): T2

  ((x: Any) => 0): T0
  ((x: Any) => 0): T2

  ((x: Any, y: Any) => 0): T0
  ((x: Any, y: Any) => 0): T1

  ((x) => 0): T2

  ((x) => 0): T0
  ((x) => 0): T2

  ((x, y) => 0): T0
  ((x, y) => 0): T1
}
