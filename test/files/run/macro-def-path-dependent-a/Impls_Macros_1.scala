import scala.reflect.macros.{Context => Ctx}

trait Exprs {
  self: Universe =>

  class Expr[T]
}

trait Reifiers {
  self: Universe =>

  type Expr[T]

  def reify[T](expr: T) = macro Impls.reify[T]
}

trait Universe extends Exprs with Reifiers

object Impls {
  def reify[T](cc: Ctx{ type PrefixType = Reifiers })(expr: cc.Expr[T]): cc.Expr[cc.prefix.value.Expr[T]] = ???
}
