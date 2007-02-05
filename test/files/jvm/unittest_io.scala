object Test {

  import scala.testing.SUnit._
  import scala.io.Source

  class ReadlinesTest extends TestCase("scala.io.Source method getLines") {

    val src = Source.fromString("""
This is a file
it is split on several lines.

isn't it?
""")
    assertEquals("wrong number of lines",src.getLines.toList.length,5) // five new lines in there
    //for(val line <- src.getLines) {
    //  Console.print(line)
    //}
  }
  def main(args:Array[String]) = {
    val ts = new TestSuite(
      new ReadlinesTest
    )
    val tr = new TestResult()
    ts.run(tr)
    for(val failure <- tr.failures) {
      Console.println(failure)
    }
  }

}
