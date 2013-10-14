import scala.tools.partest._

// java8 version of sammy_poly.scala
object Test extends CompilerTest {
  import global._

  override lazy val units: List[CompilationUnit] = {
    global.settings.Xexperimental.value = true

    // This test itself does not depend on JDK8.
    javaCompilationUnits(global)(samSource) ++
    compilationUnits(global)(useSamSource)
  }

  private def samSource = """
//   trait F[T, U] { def apply(x: T): U }
public interface F<T, U> {
    U apply(T t);
    default void yadayada() {
        throw new UnsupportedOperationException("yadayada");
    }
}
  """

  private def useSamSource = """
class T {
  def app[T, U](x: T)(f: F[T, U]): U = f(x)
  app(1)(x => List(x))
}
  """

  // We're only checking we can compile it.
  def check(source: String, unit: global.CompilationUnit): Unit = ()
}
