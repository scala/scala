import scala.tools.nsc.interactive.tests.InteractiveTest
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.interactive.Response

object Test extends InteractiveTest {
  override def execute(): Unit = {
    val sourceA = loadSourceAndWaitUntilTypechecked("A.scala")
    checkPresent(sourceA)
  }

  private def loadSourceAndWaitUntilTypechecked(sourceName: String): SourceFile = {
    val sourceFile = sourceFiles.find(_.file.name == sourceName).head
    askLoadedTyped(sourceFile).get
    /* The response to `askLoadedType` may return before `interactive.Global.waitLoadedType`
     * fully executes. Because this test expects `waitLoadedType` is fully executed before
     * calling `checkPresent`, with the below no-op presentation compiler request we make
     * sure this requirement is fulfilled.
     */
    compiler.askForResponse(() => ()).get
    sourceFile
  }

  private def checkPresent(source: SourceFile): Unit = compiler.getUnitOf(source) match {
    case Some(unit) => reporter.println("Compilation Unit for " + source.file.name + " still loaded after askLoadedTyped")

    case None => reporter.println("Test OK")
  }
}
