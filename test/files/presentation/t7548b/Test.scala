import scala.tools.nsc.interactive.tests.InteractiveTest

object Test extends InteractiveTest {
  override protected def loadSources() { /* don't parse or typecheck sources */ }

  import compiler._

  override def runDefaultTests() {
    val res = new Response[Tree]
    val pos = compiler.rangePos(sourceFiles.head, 191, 191, 191) // +
    compiler.askTypeAt(pos, res)
    res.get match {
      case Left(tree) => compiler.ask(() => reporter.println(s"$tree: ${tree.tpe}"))
      case Right(ex) => reporter.println(ex)
    }
  }
}
