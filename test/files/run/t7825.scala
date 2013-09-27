import scala.tools.partest._

object Test extends CompilerTest {
  import global._

  override lazy val units: List[CompilationUnit] = {
    // We can test this on JDK6.
    javaCompilationUnits(global)(defaultMethodSource) ++ compilationUnits(global)(scalaExtendsDefault)
  }

  private def defaultMethodSource = """
public interface Iterator<E> {
    boolean hasNext();
    E next();
    default void remove() {
        throw new UnsupportedOperationException("remove");
    }
}
  """

  private def scalaExtendsDefault = """
object Test {
  object X extends Iterator[String] {
    def hasNext = true
    def next = "!"
  }
}
  """

  // We're only checking we that the Scala compilation unit passes refchecks
  // No further checks are needed here.
  def check(source: String, unit: global.CompilationUnit): Unit = {
  }
}
