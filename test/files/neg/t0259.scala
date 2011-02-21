package test;
class TestCase3() {
  def this( groups: (String, Int)*) = this()
  def this( groups: String*) = this()
}
object Main extends TestCase3 with App
