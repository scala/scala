import scala.tools.nsc.interactive.tests.InteractiveTest
import scala.reflect.internal.util.SourceFile
import scala.tools.nsc.interactive.Response

object Test extends InteractiveTest {

  override def execute(): Unit = {
    val src = loadSourceAndWaitUntilTypechecked("Source.scala")
    checkErrors(src)
  }

  private def loadSourceAndWaitUntilTypechecked(sourceName: String): SourceFile = {
    val sourceFile = sourceFiles.find(_.file.name == sourceName).head
    askReload(List(sourceFile)).get
    askLoadedTyped(sourceFile).get
    sourceFile
  }

  private def checkErrors(source: SourceFile): Unit = compiler.getUnitOf(source) match {
    case Some(unit) =>
      val problems = unit.problems.toList
      if(problems.isEmpty) reporter.println("Test OK")
      else problems.foreach(problem => reporter.println(problem.msg))

    case None => reporter.println("No compilation unit found for " + source.file.name)
  }
}
