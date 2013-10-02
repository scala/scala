import scala.tools.partest.ReplTest

// was crashing due to a subtle interaction of the Namer entering packages into
// enclosing packages by mutating the scope in place without invalidating later
// entries in the enclosing package class symbols type history.
//
// Sadly, I couldn't whittle the test case down further.
object Test extends ReplTest {
  override def code = """val g: scala.reflect.internal.SymbolTable = null; import g.abort
                        |class C(val a: Any) extends AnyVal""".stripMargin

}
