object Test {
  import scala.testing.SUnit._

  def main(args:Array[String]): Unit = {
    val tr = new TestResult
    new TestSuite(

      new Test01

    ).run(tr)

    for(val f <- tr.failures())
      Console println f
  }

  class Foo(j:Int) {
    case class Bar(i:Int)
  }
  class Test01 extends TestCase("bir (#717 test path of case classes)") {
    val foo1 = new Foo(1)
    val foo2 = new Foo(2)

    override def runTest() = {
      val res = (foo1.Bar(2):Any) match {
       case foo2.Bar(2) => false
       case foo1.Bar(2) => true
      }
      assertTrue("ok", res);
    }
  }
}
