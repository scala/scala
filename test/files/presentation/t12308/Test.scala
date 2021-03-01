import scala.tools.nsc.interactive.tests.InteractiveTest

object Test extends InteractiveTest {

  def ws(): Unit = {
    println(compiler.unitOfFile.values.flatMap(_.problems).mkString("", "\n", ""))
  }

  override def runDefaultTests() {
    val run = compiler.currentRun

    println("askLoadedTyped 1")
    sourceFiles foreach (src => askLoadedTyped(src).get)
    ws()
    assert(run eq compiler.currentRun)

    println("askLoadedTyped 2")
    sourceFiles foreach (src => askLoadedTyped(src).get) // tree is already typed, typer is not called
    ws()
    assert(run eq compiler.currentRun)

    askReload(sourceFiles) // new run, new tree, type checking again
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

    askReload(sourceFiles)


    // what happens here:
    //   1. targeted type check of `foo`, warningin is suspended, then *not* reported because of the nowarn.
    //      once that type check is finished, `reportSuspendedMessages` is called
    //   2. targeted type check of `bar`, warning is directly issued because `reportSuspendedMessages` was called
    //      before in that run, for that source file; `suppressions` are considered known.
    //   3. targeted type check of `baz`, warning is directly issued, though it should be filtered out...
    println("targeted 2 - doesn't handle nowarn correctly")
    // tree not yet typed
    new TypeAction(compiler).runTest()
    assert(run1 ne compiler.currentRun)
    ws()
  }
}
