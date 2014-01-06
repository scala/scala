import scala.tools.partest._

object Test extends CompilerTest {
  import global._

  override lazy val units: List[CompilationUnit] = {
    // This test itself does not depend on JDK8.
    javaCompilationUnits(global)(defaultMethodSource)
  }

  private def defaultMethodSource = """
public interface Iterator<E> {
    boolean hasNext();
    E next();
    default void remove() {
        throw new UnsupportedOperationException("remove");
    }
    default void forEachRemaining(Consumer<? super E> action) {
        throw new UnsupportedOperationException("forEachRemaining");
    }
}
  """

  // We're only checking we can compile it.
  def check(source: String, unit: global.CompilationUnit): Unit = ()
}
