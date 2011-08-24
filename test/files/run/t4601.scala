class A

trait B {
  self: A =>

  def test {
    println('blubber)
  }
}

object Test extends A with B {
  def main(args: Array[String]) {
    test
  }
}