package scala.reflect.makro

import language.experimental.macros
import scala.reflect.api.{Universe => ApiUniverse}

// anchors for materialization macros emitted during tag materialization in Implicits.scala
// implementation is magically hardwired into `scala.reflect.reify.Taggers`
//
// todo. once we have implicit macros for tag generation, we can remove these anchors
// [Eugene++] how do I hide this from scaladoc?
package object internal {
  private[scala] def materializeArrayTag[T](u: ApiUniverse): ArrayTag[T] = macro ???
  private[scala] def materializeErasureTag[T](u: ApiUniverse): ErasureTag[T] = macro ???
  private[scala] def materializeClassTag[T](u: ApiUniverse): ClassTag[T] = macro ???
  private[scala] def materializeTypeTag[T](u: ApiUniverse): u.TypeTag[T] = macro ???
  private[scala] def materializeConcreteTypeTag[T](u: ApiUniverse): u.ConcreteTypeTag[T] = macro ???
}
