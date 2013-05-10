import scala.tools.partest._

object Test extends CompilerTest {
  import global._

  def javaVersion = scala.util.Properties.javaVersion
  def isJavaEight = javaVersion startsWith "1.8"
  // This way we auto-pass on non-java8 since there's nothing to check
  override lazy val units = {
    val res: List[CompilationUnit]  = if (isJavaEight) javaCompilationUnits(global)(defaultMethodSource) else Nil
    val word = if (isJavaEight) "Attempting" else "Skipping"
    log(s"$word java8-specific test under java version $javaVersion")
    res
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
