import scala.tools.nsc.interactive.tests.InteractiveTest

// also known as SI-5013

object Test extends InteractiveTest {

  override def runDefaultTests(): Unit = {
    val a = sourceFiles.find(_.file.name == "a.scala").head
    val b = sourceFiles.find(_.file.name == "b.scala").head
    askLoadedTyped(a).get
    askLoadedTyped(b).get
    super.runDefaultTests()
  }

}
