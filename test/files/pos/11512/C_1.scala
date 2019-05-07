trait T { this: U =>
  def m: Int
}
trait U {
  def m: Int = ???
}
abstract class C extends U with T
