import java.util.concurrent.CountDownLatch
import scala.tools.nsc.interactive.Global
import scala.tools.nsc.interactive.tests.InteractiveTest

// A completion in `Use` is served while background type checking is still in `Before`.
// The full type check that follows must still find the implicit conversion to `RichInt`.
object Test extends InteractiveTest {
  val reached, posted = new CountDownLatch(1)

  override protected def createGlobal: Global = new Global(settings, compilerReporter) {
    override def signalDone(context: Context, old: Tree, result: Tree): Unit = {
      if (onCompilerThread && reached.getCount > 0 && interruptsEnabled && lockedCount == 0) {
        reached.countDown()
        posted.await()
      }
      super.signalDone(context, old, result)
    }
  }

  override protected def runDefaultTests(): Unit = {
    import compiler._
    val src = sourceFiles.head
    reached.await()
    val completion = new Response[List[Member]]
    askTypeCompletion(src.position(new String(src.content).indexOf("/*!*/") - 1), completion)
    posted.countDown()
    completion.get
    val loaded = new Response[Tree]
    compiler.askLoadedTyped(src, loaded)
    loaded.get
    println(unitOfFile(src.file).problems.mkString("\n"))
  }
}
