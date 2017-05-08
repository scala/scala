package test.scaladoc.template.owners

trait X {
  /** @template */
  type Symbol >: Null <: SymbolApi

  /** @template */
  type TypeSymbol >: Null <: Symbol with TypeSymbolApi

  /** @template */
  type TermSymbol >: Null <: Symbol with TermSymbolApi

  /** @template */
  type MethodSymbol >: Null <: TermSymbol with MethodSymbolApi

  trait SymbolApi { this: Symbol => def x: Int}
  trait TermSymbolApi extends SymbolApi { this: TermSymbol => def y: Int}
  trait TypeSymbolApi extends SymbolApi { this: TypeSymbol => def z: Int}
  trait MethodSymbolApi extends TermSymbolApi { this: MethodSymbol => def t: Int }
}

trait Y extends X
trait Z extends Y
trait T extends Z
