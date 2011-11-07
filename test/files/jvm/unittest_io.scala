import scala.testing.SUnit._
import scala.io.Source

object Test extends TestConsoleMain {

  def suite = new TestSuite(
      new ReadlinesTest
  )

  class ReadlinesTest extends TestCase("scala.io.Source method getLines()") {
    
    val src = Source.fromString(""" 
This is a file
it is split on several lines.

isn't it?
""")
    def runTest() = assertEquals("wrong number of lines",src.getLines.toList.length,5) // five new lines in there
    //for (line <- src.getLines) {
    //  Console.print(line)
    //}
  }

}
