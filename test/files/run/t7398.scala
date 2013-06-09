import scala.tools.partest._

object Test extends CompilerTest {
  import global._

  // This way we auto-pass on non-java8 since there's nothing to check
  override lazy val units: List[CompilationUnit]  = testUnderJavaAtLeast("1.8") {
    javaCompilationUnits(global)(defaultMethodSource)
  } otherwise {
    Nil
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

  // We're only checking we can parse it.
  def check(source: String, unit: global.CompilationUnit): Unit = ()
}
