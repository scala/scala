import scala.tools.nsc._
import scala.tools.partest.CompilerTest
import scala.collection.{ mutable, immutable, generic }

object Test extends CompilerTest {
  import global._
  import rootMirror._
  import definitions._
  import global.analyzer.{Context, ImportInfo}

  override def code = """
package context {
}
  """

  def check(source: String, unit: global.CompilationUnit) = {
    val context: Context = global.analyzer.rootContext(unit)
    val importInfo: ImportInfo = context.imports.head // Predef._
    val importedSym = importInfo.importedSymbol(termNames.CONSTRUCTOR)
    assert(importedSym == NoSymbol, importedSym) // was "constructor Predef"
  }
}
