package compilerbug

class TestClass {
  def repro() {
    SadObject.buggyMethod[Int]()()
  }
}
