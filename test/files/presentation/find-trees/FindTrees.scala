import scala.tools.nsc.interactive.tests.InteractiveTest
import scala.tools.nsc.util.Position


/** Example interactive test that does everything by hand. It would be much simpler
 *  to just add the markers in the test file. This test shows how to drive
 *  the presentation compiler manually.
 */
object Test extends InteractiveTest {

  def askForPos(pos: Position) {
    import compiler._
    val response = new Response[Tree]

    println("asking position at %d:%d".format(pos.line, pos.column))
    compiler.askTypeAt(pos, response)
    response.get match {
      case Left(EmptyTree) =>
        println("error retrieving tree at %d:%d".format(pos.line, pos.column))
      case Left(t) =>
        println("retrieved tree: " + t)
    }
    println(this.reporter.infos.mkString("\n"))
  }

  override def runTest {
    import compiler._
    val src = sourceFiles(0) // only one under src/
    val pos = rangePos(src, 426, 426, 433)
    val pos1 = src.position(19, 15)

    // reload is issued already by the framework, but we can redo it here as an example
    val reload = new Response[Unit]
    compiler.askReload(List(src), reload)
    reload.get // it's important to let reload finish before asking other things.

    askForPos(pos)
    println("=" * 20)
    askForPos(pos1)

    compiler.askShutdown()
  }
}
