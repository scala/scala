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
  }

  // You can enable settings for the presentation compiler here
  // but don't leave them in the nightly build since the log will most likely
  // contain absolute paths

//  settings.YpresentationDebug.value = true
//  settings.YpresentationVerbose.value = true

  override def runTest {
    import compiler._
    val src = sourceFiles(0) // only one under src/
    val pos = rangePos(src, 426, 426, 433)
    val pos1 = src.position(19, 15)  // this is an offset position

    // reload is issued already by the framework, so we don't need to do it, but it doesn't hurt
    val reload = new Response[Unit]
    compiler.askReload(List(src), reload)
    reload.get // it's important to let reload finish before asking other things.

    // re-enable when positions in the primary constructor are handled reliably
    askForPos(pos)
    println("=" * 20)
    askForPos(pos1)

    compiler.askShutdown()
  }
}
