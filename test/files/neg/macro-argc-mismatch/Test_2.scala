import Macros._

object Test extends App {
  one
  one()
  one(2, 3)
  one()()
  one(1)()

  two
  two()
  two(2, 3)
  two()()
  two(1)
  two(1)()
  two(1)(2, 3)
  two(1)()()
  two(1)(1)()
}