
trait T {
  val adder0: Int => Int = _ + 3   // Works fine
  var adder1: Int => Int = (_ + 3) // Works fine
  var adder2: Int => Int = _ + 3   // was: Error
}
