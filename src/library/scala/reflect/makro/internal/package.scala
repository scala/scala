package scala.reflect
package makro

import scala.reflect.base.{Universe => BaseUniverse}

package object internal {
  private[scala] type macroImpl = scala.reflect.macros.internal.macroImpl
  private[scala] def materializeClassTag[T](u: BaseUniverse): ClassTag[T] = ??? // macro
  private[scala] def materializeAbsTypeTag[T](u: BaseUniverse): u.AbsTypeTag[T] = ??? // macro
  private[scala] def materializeTypeTag[T](u: BaseUniverse): u.TypeTag[T] = ??? // macro
}
