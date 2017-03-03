
import tools.partest.DirectTest
import reflect.internal.util._

// mimic the resident compiler failure by recompiling
// the class with new run of same global.
object Test extends DirectTest {

  override def code = """
    object Main {
      def main(args: Array[String]): Unit = {
        Surf xmain args
        import trial.core.Rankable
        object Surf {
          def xmain(args: Array[String]): Unit = println(new Strategy("win").rank)
        }
        class Strategy(name:String) extends Rankable
      }
    }
  """

  override def show(): Unit = {
    // first, compile the interface
    val dependency = """
    |package trial
    |
    |object core {
    |  trait Rankable {
    |    val rank: String = "high"
    |  }
    |}
    |""".stripMargin

    assert(compileString(newCompiler())(dependency))

    // a resident global
    val g = newCompiler()

    assert(compileString(g)(code))
    ScalaClassLoader(getClass.getClassLoader) run ("Main", Nil)
    assert(compileString(g)(code))
    ScalaClassLoader(getClass.getClassLoader) run ("Main", Nil)
  }

  override def extraSettings = s"-usejavacp -d ${testOutput.path}"
}
