import scala.runtime.BoxesRunTime

// This test checks whether the ICodeReader bugs are able to affect
// the inlining. They prevented the build from succeeding before
// merging the patch to inline traits. If you compile this with
// -optimize -Ydebug -Ylog:inliner you will notice a couple of stack
// traces -- that's ICodeReader crashing thus bytecode not being
// available
object Test {
  def main(args: Array[String]) = {
    val x: scala.Int = 3
    val y: scala.Int = 2
    BoxesRunTime.shiftLogicalRight(3, 2)
    ()
  }
}
