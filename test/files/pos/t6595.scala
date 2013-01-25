import scala.annotation.switch

class Foo extends {
  final val b0 = 5
} with AnyRef {
  final val b1 = 10

  // Using the @switch annotation as a means of testing that the
  // type inferred for b0 is Int(5) and not Int. Only in the former
  // case can a switch be generated.
  def f(p: Int) = (p: @switch) match {
    case `b0` => 1
    case `b1` => 2
    case 15   => 3
    case 20   => 4
    case _    => 5
  }
}
