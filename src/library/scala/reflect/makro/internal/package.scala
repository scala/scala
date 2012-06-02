package scala.reflect
package makro

import language.experimental.macros
import scala.reflect.base.{Universe => BaseUniverse}

// anchors for materialization macros
// emitted during tag materialization in Implicits.scala
// implementation is magically hardwired into `scala.reflect.reify.Taggers`
//
// don't scoff at the "magically" part - fast track would hardcode the implementation anyways
// and this way we ensure that related logic (tag materializers) stays in related package (scala.reflect.reify)
//
// todo. once we have implicit macros for tag generation, we can remove these anchors
// [Eugene++] how do I hide this from scaladoc?
package object internal extends internal_compat {
  private[scala] def materializeArrayTag[T](u: BaseUniverse): ArrayTag[T] = macro materializeArrayTag_impl[T]
  private[scala] def materializeArrayTag_impl[T: c.TypeTag](c: Context)(u: c.Expr[BaseUniverse]): c.Expr[ArrayTag[T]] = ???

  private[scala] def materializeClassTag[T](u: BaseUniverse): ClassTag[T] = macro materializeClassTag_impl[T]
  private[scala] def materializeClassTag_impl[T: c.TypeTag](c: Context)(u: c.Expr[BaseUniverse]): c.Expr[ClassTag[T]] = ???

  private[scala] def materializeTypeTag[T](u: BaseUniverse): u.TypeTag[T] = macro materializeTypeTag_impl[T]
  private[scala] def materializeTypeTag_impl[T: c.TypeTag](c: Context)(u: c.Expr[BaseUniverse]): c.Expr[u.value.TypeTag[T]] = ???

  private[scala] def materializeConcreteTypeTag[T](u: BaseUniverse): u.ConcreteTypeTag[T] = macro materializeConcreteTypeTag_impl[T]
  private[scala] def materializeConcreteTypeTag_impl[T: c.TypeTag](c: Context)(u: c.Expr[BaseUniverse]): c.Expr[u.value.ConcreteTypeTag[T]] = ???
}
