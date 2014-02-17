trait Universe {
  type Symbol >: Null <: AnyRef with SymbolApi
  trait SymbolApi

  type TypeSymbol >: Null <: TypeSymbolApi with Symbol
  trait TypeSymbolApi

  implicit class CompatibleSymbol(sym: Symbol) {
    def asFreeType: TypeSymbol = ???
  }
}

object Test extends App {
  val u: Universe = ???
  import u._

  val sym: Symbol = ???
  sym.asFreeType
}
