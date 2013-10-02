abstract class NullaryTest[T, m[s]] {
  def nullary: String = "a"
  val x = nullary

  def nullary2: T
  val x2 = nullary2

  def nullary3: m[T]
  val x3 = nullary3
}

class Concrete extends NullaryTest[Int, List] {
        def nullary2 = 1
        def nullary3 = List(1,2,3)
}

object test {
        (new Concrete).nullary2
        (new Concrete).nullary3
}
