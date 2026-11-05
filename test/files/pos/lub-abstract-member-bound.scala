// `NameType` was ranked by its declared bound `Api#Name`, so the class `Name`, with its longer
// base type sequence, ranked ahead of it. The lub lost `Name`: found `A with B with C with D`.
trait Api {
  type Name >: Null <: AnyRef
  trait Sym { type NameType >: Null <: Name; def name: NameType }
}
trait Impl extends Api {
  trait A; trait B; trait C; trait D
  abstract class Name extends A with B with C with D
  class TermName extends Name
  def t: TermName
  def s: Sym
  def f(b: Boolean) = { val m = if (b) t else s.name; val n: Name = m; n }
}

// A tie in rank, broken by symbol id: this failed only with `Impl2` declared before `Api2`.
trait Impl2 extends Api2 {
  trait X; trait Y
  abstract class Name extends NameApi with X with Y
  class TermName extends Name
  def t: TermName
  def s: Sym
  def f(b: Boolean) = { val m = if (b) t else s.name; val n: Name = m; n }
}
trait Api2 {
  type Name >: Null <: AnyRef with NameApi
  trait NameApi
  trait Sym { type NameType >: Null <: Name; def name: NameType }
}
