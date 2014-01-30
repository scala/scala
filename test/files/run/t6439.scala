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
object lookup {
  import intp._
  def apply(name: String): Symbol       = types(name) orElse terms(name)
  def types(name: String): Symbol       = replScope lookup (TypeName(name)) orElse getClassIfDefined(name)
  def terms(name: String): Symbol       = replScope lookup (TermName(name)) orElse getModuleIfDefined(name)
  def types[T: global.TypeTag] : Symbol = typeOf[T].typeSymbol
  def terms[T: global.TypeTag] : Symbol = typeOf[T].termSymbol
  def apply[T: global.TypeTag] : Symbol = typeOf[T].typeSymbol
}
lookup("F") // this now works as a result of changing .typeSymbol to .typeSymbolDirect in IMain#Request#definedSymbols
             """
}
