package scala.reflect
package makro

import language.experimental.macros
import scala.reflect.base.{Universe => BaseUniverse}

// anchors for materialization macros emitted during tag materialization in Implicits.scala
// implementation is magically hardwired into `scala.reflect.reify.Taggers`
//
// todo. once we have implicit macros for tag generation, we can remove these anchors
// [Eugene++] how do I hide this from scaladoc?
package object internal {
  private[scala] def materializeClassTag[T](u: BaseUniverse): ClassTag[T] = macro ???
  private[scala] def materializeAbsTypeTag[T](u: BaseUniverse): u.AbsTypeTag[T] = macro ???
  private[scala] def materializeTypeTag[T](u: BaseUniverse): u.TypeTag[T] = macro ???
}
