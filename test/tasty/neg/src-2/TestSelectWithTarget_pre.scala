package tastytest

import scala.language.experimental.macros

import scala.reflect.macros.blackbox.Context

object TestSelectWithTargetPre {

  /** forces annotations of type `A` on methods from class `T` */
  def forceAnnots[T, A]: Unit = macro Macros.forceAnnotsImpl[T, A]

  object Macros {
    def forceAnnotsImpl[T, A](c: Context)(implicit T: c.WeakTypeTag[T], A: c.WeakTypeTag[A]): c.Expr[Unit] = {
      import c.universe._
      for {
        method <- weakTypeOf[T].members.filter(_.isMethod)
        annot <- method.annotations.find(_.tree.tpe =:= weakTypeOf[A])
      } {
        annot.tree
      }
      c.Expr[Unit](q"()")
    }
  }

}
