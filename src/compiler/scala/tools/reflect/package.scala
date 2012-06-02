/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools

import java.lang.reflect.Method
import java.{ lang => jl }
import scala.reflect.{ ClassTag, classTag }
import scala.reflect.api.JavaUniverse
import language.implicitConversions

package object reflect extends FrontEnds {
  def nameAndArity(m: Method) = (m.getName, m.getParameterTypes.size)
  def allInterfaces(cl: Class[_]): List[Class[_]] =
    if (cl == null) Nil
    else cl.getInterfaces.toList ++ allInterfaces(cl.getSuperclass).distinct

  def methodsNamed(target: AnyRef, name: String): List[Method] =
    target.getClass.getMethods.toList filter (x => x.getName == name)

  /** If there is a single non-bridge apply method in the given instance,
   *  return it: otherwise None.
   */
  def uniqueApply(target: AnyRef) = {
    methodsNamed(target, "apply") filterNot (_.isBridge) match {
      case List(x)  => Some(x)
      case _        => None
    }
  }

  def zeroOfClass(clazz: Class[_]) = zeroOf(ClassTag(clazz))
  def zeroOf[T](implicit t: ClassTag[T]): AnyRef = {
    if (t == classTag[Boolean] || t == classTag[jl.Boolean]) false: jl.Boolean
    else if (t == classTag[Unit] || t == classTag[jl.Void] || t == classTag[scala.runtime.BoxedUnit]) scala.runtime.BoxedUnit.UNIT
    else if (t == classTag[Char] || t == classTag[jl.Character]) 0.toChar: jl.Character
    else if (t == classTag[Byte] || t == classTag[jl.Byte]) 0.toByte: jl.Byte
    else if (t == classTag[Short] || t == classTag[jl.Short]) 0.toShort: jl.Short
    else if (t == classTag[Int] || t == classTag[jl.Integer]) 0: jl.Integer
    else if (t == classTag[Long] || t == classTag[jl.Long]) 0l: jl.Long
    else if (t == classTag[Float] || t == classTag[jl.Float]) 0f: jl.Float
    else if (t == classTag[Double] || t == classTag[jl.Double]) 0d: jl.Double
    else null
  }

  // [todo: can we generalize this?
  import scala.reflect.runtime.{universe => ru}
  implicit def ToolBox(mirror0: ru.Mirror): ToolBoxFactory[ru.type] =
    new ToolBoxFactory[ru.type](mirror0.universe) {
      lazy val mirror = mirror0
    }

  // todo. replace this with an implicit class, once the pesky warning is gone
  implicit def Eval[T](expr: JavaUniverse # Expr[T]): Eval[T] = new Eval[T](expr)

  // we don't provide `Eval` for trees, because it's unclear where to get an evaluation mirror from
}

package reflect {
  class Eval[T](expr: JavaUniverse # Expr[T]) {
    def eval: T = {
      val factory = new ToolBoxFactory[JavaUniverse](expr.mirror.universe) { val mirror = expr.mirror.asInstanceOf[this.u.Mirror] }
      val toolBox = factory.mkToolBox()
      toolBox.runExpr(expr.tree.asInstanceOf[toolBox.u.Tree]).asInstanceOf[T]
    }
  }
}
