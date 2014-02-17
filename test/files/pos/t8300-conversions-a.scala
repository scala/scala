// cf. pos/t8300-conversions-b.scala
trait Universe {
  type Symbol >: Null <: AnyRef with SymbolApi
  trait SymbolApi

  type TypeSymbol >: Null <: Symbol with TypeSymbolApi
  trait TypeSymbolApi extends SymbolApi

  type FreeTypeSymbol >: Null <: TypeSymbol with FreeTypeSymbolApi
  trait FreeTypeSymbolApi extends TypeSymbolApi

  implicit class CompatibleSymbol(sym: Symbol) {
    def asFreeType: FreeTypeSymbol = ???
  }
}

object Test extends App {
  val u: Universe = ???
  import u._

  val sym: Symbol = ???
  sym.asFreeType
}