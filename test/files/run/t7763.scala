object Test {
  class A; class B
  def main(args: Array[String]) {
    def noExpectedType() {
      a().asInstanceOf[B] // cast elided!
    }
    def withExpectedType(): B = {
      a().asInstanceOf[B]
    }
    def test(a: => Any) = try {
      a
      sys.error("no CCE!")
    } catch {case _: ClassCastException => }

    test(noExpectedType())
    test(withExpectedType())
  }

  def a(): Object = new A
}
