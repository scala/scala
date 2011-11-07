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

class A2

trait B2 {
  self: A2 =>

  def test {
    import Pimper.pimp

    println(5.test)
  }
}

object Test extends A with B {
  def main(args: Array[String]) {
    test
    Test2.test
  }
}

object Test2 extends A2 with B2 

