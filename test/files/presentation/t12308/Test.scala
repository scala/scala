import scala.tools.nsc.interactive.tests.InteractiveTest

object Test extends InteractiveTest {

  def ws(): Unit = {
    println(compiler.unitOfFile.values.flatMap(_.problems).mkString("", "\n", ""))
  }

  override def runDefaultTests(): Unit = {
    val run = compiler.currentRun

    println("askLoadedTyped 1")
    sourceFiles foreach (src => askLoadedTyped(src).get)
    ws()
    assert(run eq compiler.currentRun)

    println("askLoadedTyped 2")
    sourceFiles foreach (src => askLoadedTyped(src).get) // tree is already typed, typer is not called
    ws()
    assert(run eq compiler.currentRun)

    askReload(sourceFiles.toIndexedSeq) // new run, new tree, type checking again
    println("askLoadedTyped 3")
    sourceFiles foreach (src => askLoadedTyped(src).get)
    ws()
    val run1 = compiler.currentRun
    assert(run ne run1)

    println("targeted 1")
    // tree is already typed, typer not called
    new TypeAction(compiler).runTest()
    assert(run1 eq compiler.currentRun)
    ws()

    askReload(sourceFiles.toIndexedSeq)

    // Targeted type checks keep their warnings suspended, the full type check filters them with all `@nowarn`s known.
    // Background type checking may run before or after the targeted checks, the outcome is the same.
    println("targeted 2")
    // tree not yet typed
    new TypeAction(compiler).runTest()
    assert(run1 ne compiler.currentRun)
    sourceFiles foreach (src => askLoadedTyped(src).get)
    ws()
  }
}
