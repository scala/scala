//> using options -Werror

// Distinct classes of the same name under a self type (scala/bug#13205) are distinct in pattern matching:
// neither type test makes the other unreachable, and each tests its own class.
trait A { class C; type AC = C; def mkA: AC = new C }
trait B { class C; type BC = C; def mkB: BC = new C }
class D1
class D2
class D3
class D4
class D5
class D6
class D7
class D8
class D9
class D10
class D11
class D12
class D13
class D14
class D15
class D16
class D17
class D18
class D19
class D20
trait X { self: A with B =>
  def small(x: Any): Int = x match {
    case _: BC => 1
    case _: AC => 2
    case _ => 0
  }
  def smallFlipped(x: Any): Int = x match {
    case _: AC => 2
    case _: BC => 1
    case _ => 0
  }
  // more than 16 type tests
  def big(x: Any): Int = x match {
    case _: D1 => -1
    case _: D2 => -2
    case _: D3 => -3
    case _: D4 => -4
    case _: D5 => -5
    case _: D6 => -6
    case _: D7 => -7
    case _: D8 => -8
    case _: D9 => -9
    case _: D10 => -10
    case _: D11 => -11
    case _: D12 => -12
    case _: D13 => -13
    case _: D14 => -14
    case _: D15 => -15
    case _: D16 => -16
    case _: D17 => -17
    case _: D18 => -18
    case _: D19 => -19
    case _: D20 => -20
    case _: BC => 1
    case _: AC => 2
    case _ => 0
  }
}

object Test extends App {
  val x = new X with A with B
  for ((name, f) <- List[(String, Any => Int)](("small", x.small), ("smallFlipped", x.smallFlipped), ("big", x.big)))
    println(s"$name: ${f(x.mkA)} ${f(x.mkB)} ${f("")}")
}
