// The lub of `x.sym.NameType forSome { val x: SM }` and `NoSymbol.NameType` is `Name`.
// `SymbolApi#NameType`'s declared bound is the abstract `Api#Name`, but in `Impl` its bound is the class `Name`,
// which has the same depth. The base type sequence of `sym.NameType` lists `NameType` before `Name`, so `isLess`
// must order abstract types before classes of the same depth (as `Impl.this.Name` precedes `NameType` by name).
trait Api {
  type Name >: Null <: AnyRef with NameApi
  trait NameApi
  trait SymbolApi {
    type NameType >: Null <: Name
    def name: NameType
  }
}

trait Impl extends Api {
  trait Extra; trait Extra2
  abstract class Name extends NameApi with Extra with Extra2
  abstract class TermName extends Name
  abstract class Symbol extends SymbolApi
  object NoSymbol extends Symbol {
    type NameType = TermName
    def name: TermName = null
  }
  case class SM(sym: Symbol)

  def f(o: Option[SM]): Name = o.map(_.sym.name).getOrElse(NoSymbol.name)
}
