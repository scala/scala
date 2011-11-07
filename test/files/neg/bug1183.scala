import scala.testing.SUnit._

object Test extends TestConsoleMain {

  def suite = new TestSuite(
      new Test717
    )

  class Foo(j:Int) {
    object Baz
    class Bam
    object Bar
    case class Bar(i:Int)
  }

  
  class Test717 extends TestCase("#717 test path of case classes") {
    val foo1 = new Foo(1)

    override def runTest() = {
      val res = (foo1.Bar(2):Any) match {
        case foo1.Bar(2) => true   // (1)
      }
      assertTrue("ok", res);
    }
  }

  // (2)
  object Foo {
    class Bar(val x : String)
    class Baz
    object Bam
    object Bar
    
    def unapply(s : String) : Option[Bar] = Some(new Bar(s))
  }

}
