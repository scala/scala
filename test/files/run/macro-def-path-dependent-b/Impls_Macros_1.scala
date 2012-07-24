import scala.reflect.macros.{Context => Ctx}

trait Exprs {
  self: Universe =>

  class Expr[T]
}

trait Reifiers {
  self: Universe =>

}

trait Universe extends Exprs with Reifiers {
  def reify[T](expr: T) = macro Impls.reify[T]
}

object Impls {
  def reify[T](cc: Ctx{ type PrefixType = Universe })(expr: cc.Expr[T]): cc.Expr[cc.prefix.value.Expr[T]] = ???
}
