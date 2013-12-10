// NOTE: blocked by SI-8049

// package test1
//
// import scala.reflect.macros.{BlackboxContext => Ctx}
//
// trait Exprs {
//   self: Universe =>
//
//   class Expr[T]
// }
//
// trait Reifiers {
//   self: Universe =>
//
//   type Expr[T]
//
//   def reify[T](expr: T): Expr[T] = macro Impls.reify[T]
// }
//
// trait Universe extends Exprs with Reifiers
//
// object Impls {
//   def reify[T](cc: Ctx{ type PrefixType = Reifiers })(expr: cc.Expr[T]): cc.Expr[cc.prefix.value.Expr[T]] = ???
// }
