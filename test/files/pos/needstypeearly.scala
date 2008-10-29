abstract class NeedsXEarly {
  val x: Int
}
class Foo extends { val x = 1 } with NeedsXEarly
