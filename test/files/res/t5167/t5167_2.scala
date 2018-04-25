package compilerbug

class TestClass {
  def repro(): Unit = {
    SadObject.buggyMethod[Int]()()
  }
}
