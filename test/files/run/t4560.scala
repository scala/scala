object Pimper {
 implicit def pimp(i: Int) = new {
    def test: String = i.toString
  }
}

trait A

trait B {
  self: A =>

  def test {
    import Pimper.pimp

    println(5.test)
  }
}

object Test extends A with B {
  def main(args: Array[String]) {
    test
  }
}
