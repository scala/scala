import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
class A
object A // warn
trait B
object B // warn
object C
object Bippy
class C // warn
class D
def D = 0 // no warn
val D = 0 // no warn
object E
var E = 0 // no warn
object F
type F = Int // no warn
:power
intp("F") // this now works as a result of changing .typeSymbol to .typeSymbolDirect in IMain#Request#definedSymbols
             """
}
