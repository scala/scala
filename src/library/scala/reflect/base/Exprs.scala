/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package base

trait Exprs { self: Universe =>

  /** An expression tree tagged with its type */
  trait Expr[+T] extends Equals with Serializable {
    val mirror: Mirror
    def in[U <: Universe with Singleton](otherMirror: MirrorOf[U]): U # Expr[T]

    def tree: Tree
    def staticType: Type
    def actualType: Type

    def splice: T
    val value: T

    /** case class accessories */
    override def canEqual(x: Any) = x.isInstanceOf[Expr[_]]
    override def equals(x: Any) = x.isInstanceOf[Expr[_]] && this.mirror == x.asInstanceOf[Expr[_]].mirror && this.tree == x.asInstanceOf[Expr[_]].tree
    override def hashCode = mirror.hashCode * 31 + tree.hashCode
    override def toString = "Expr["+staticType+"]("+tree+")"
  }

  object Expr {
    def apply[T: AbsTypeTag](mirror: MirrorOf[self.type], treec: TreeCreator): Expr[T] = new ExprImpl[T](mirror.asInstanceOf[Mirror], treec)
    def unapply[T](expr: Expr[T]): Option[Tree] = Some(expr.tree)
  }

  private class ExprImpl[+T: AbsTypeTag](val mirror: Mirror, val treec: TreeCreator) extends Expr[T] {
    def in[U <: Universe with Singleton](otherMirror: MirrorOf[U]): U # Expr[T] = {
      val otherMirror1 = otherMirror.asInstanceOf[MirrorOf[otherMirror.universe.type]]
      val tag1 = (implicitly[AbsTypeTag[T]] in otherMirror).asInstanceOf[otherMirror.universe.AbsTypeTag[T]]
      otherMirror.universe.Expr[T](otherMirror1, treec)(tag1)
    }

    lazy val tree: Tree = treec(mirror)
    lazy val staticType: Type = implicitly[AbsTypeTag[T]].tpe
    def actualType: Type = treeType(tree)

    def splice: T = throw new UnsupportedOperationException("""
      |the function you're calling has not been spliced by the compiler.
      |this means there is a cross-stage evaluation involved, and it needs to be invoked explicitly.
      |if you're sure this is not an oversight, add scala-compiler.jar to the classpath,
      |import `scala.tools.reflect.Eval` and call `<your expr>.eval` instead.""".trim.stripMargin)
    lazy val value: T = throw new UnsupportedOperationException("""
      |the value you're calling is only meant to be used in cross-stage path-dependent types.
      |if you want to splice the underlying expression, use `<your expr>.splice`.
      |if you want to get a value of the underlying expression, add scala-compiler.jar to the classpath,
      |import `scala.tools.reflect.Eval` and call `<your expr>.eval` instead.""".trim.stripMargin)

    private def writeReplace(): AnyRef = new SerializedExpr(treec, implicitly[AbsTypeTag[T]].in(scala.reflect.basis.rootMirror))
  }
}

private[scala] class SerializedExpr(var treec: TreeCreator, var tag: scala.reflect.basis.AbsTypeTag[_]) extends Serializable {
  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeObject(treec)
    out.writeObject(tag)
  }

  private def readObject(in: java.io.ObjectInputStream): Unit = {
    treec = in.readObject().asInstanceOf[TreeCreator]
    tag = in.readObject().asInstanceOf[scala.reflect.basis.AbsTypeTag[_]]
  }

  private def readResolve(): AnyRef = {
    import scala.reflect.basis._
    Expr(rootMirror, treec)(tag)
  }
}