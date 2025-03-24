/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package api

import scala.reflect.runtime.{universe => ru}
import scala.annotation.compileTimeOnly
import java.io.ObjectStreamException

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * A trait that defines strongly-typed tree wrappers and operations on them for use in Scala Reflection.
 *
 *  `Expr` wraps an abstract syntax tree ([[scala.reflect.api.Trees#Tree]]) and tags it with its type ([[scala.reflect.api.Types#Type]]).
 *
 *  Usually `Expr`s are created via [[scala.reflect.api.Universe#reify]], in which case a compiler
 *  produces a [[scala.reflect.api.TreeCreator]] for the provided expression and also
 *  creates a complementary [[scala.reflect.api.TypeTags#WeakTypeTag]] that corresponds to the type of that expression.
 *
 * `Expr`s can also be created manually via the `Expr` companion object, but then the burden of providing a `TreeCreator` lies on the programmer.
 *  Compile-time reflection via macros, as described in [[scala.reflect.macros.Aliases]], provides an easier way to instantiate exprs manually.
 *  Manual creation, however, is very rarely needed when working with runtime reflection.
 *
 *  `Expr` can be migrated from one mirror to another by using the `in` method. Migration means that all symbolic references
 *  to classes/objects/packages in the expression are re-resolved within the new mirror
 *  (typically using that mirror's classloader). The default universe of an `Expr` is typically
 *  [[scala.reflect.runtime#universe]], the default mirror is typically [[scala.reflect.runtime#currentMirror]].
 *
 * @group ReflectionAPI
 */
trait Exprs { self: Universe =>

  /** Expr wraps an abstract syntax tree and tags it with its type.
   *  The main source of information about exprs is the [[scala.reflect.api.Exprs]] page.
   *  @group Expressions
   */
  trait Expr[+T] extends Equals with Serializable {
    /**
     * Underlying mirror of this expr.
     */
    val mirror: Mirror

    /**
     * Migrates the expression into another mirror, jumping into a different universe if necessary.
     */
    def in[U <: Universe with Singleton](otherMirror: scala.reflect.api.Mirror[U]): U # Expr[T]

    /**
     * The Scala abstract syntax tree representing the wrapped expression.
     */
    def tree: Tree

    /**
     * Type of the wrapped expression tree as provided during creation.
     *
     * When exprs are created by the compiler, `staticType` represents
     * a statically known type of the tree as calculated at that point by the compiler.
     */
    def staticType: Type

    /**
     * Type of the wrapped expression tree as found in the underlying tree.
     */
    def actualType: Type

    /**
     * A dummy method to mark expression splicing in reification.
     *
     * It should only be used within a `reify` call, which eliminates the `splice` call and embeds
     * the wrapped tree into the reified surrounding expression.
     * If used alone `splice` throws an exception when called at runtime.
     *
     * If you want to use an Expr in reification of some Scala code, you need to splice it in.
     * For an expr of type `Expr[T]`, where `T` has a method `foo`, the following code
     * {{{
     *   reify{ expr.splice.foo }
     * }}}
     * uses splice to turn an expr of type Expr[T] into a value of type T in the context of `reify`.
     *
     * It is equivalent to
     * {{{
     *   Select( expr.tree, TermName("foo") )
     * }}}
     *
     * The following example code however does not compile
     * {{{
     *   reify{ expr.foo }
     * }}}
     * because expr of type Expr[T] itself does not have a method foo.
     */
    @compileTimeOnly("splice must be enclosed within a reify {} block")
    def splice: T

    /**
     * A dummy value to denote cross-stage path-dependent type dependencies.
     *
     * For example for the following macro definition:
     * {{{
     * class X { type T }
     * object Macros { def foo(x: X): x.T = macro Impls.foo_impl }
     * }}}
     *
     * The corresponding macro implementation should have the following signature (note how the return type denotes path-dependency on x):
     * {{{
     * object Impls { def foo_impl(c: Context)(x: c.Expr[X]): c.Expr[x.value.T] = ... }
     * }}}
     */
    @compileTimeOnly("cannot use value except for signatures of macro implementations")
    val value: T

    override def canEqual(x: Any) = x.isInstanceOf[Expr[_]]

    override def equals(x: Any) = x.isInstanceOf[Expr[_]] && this.mirror == x.asInstanceOf[Expr[_]].mirror && this.tree == x.asInstanceOf[Expr[_]].tree

    override def hashCode = mirror.hashCode * 31 + tree.hashCode

    override def toString = "Expr["+staticType+"]("+tree+")"
  }

  /**
   * Constructor/Extractor for Expr.
   *
   * Can be useful, when having a tree and wanting to splice it in reify call,
   * in which case the tree first needs to be wrapped in an expr.

   * The main source of information about exprs is the [[scala.reflect.api.Exprs]] page.
   *  @group Expressions
   */
  object Expr {
    def apply[T: WeakTypeTag](mirror: scala.reflect.api.Mirror[self.type], treec: TreeCreator): Expr[T] = new ExprImpl[T](mirror.asInstanceOf[Mirror], treec)
    def unapply[T](expr: Expr[T]): Option[Tree] = Some(expr.tree)
  }

  private class ExprImpl[+T: WeakTypeTag](val mirror: Mirror, val treec: TreeCreator) extends Expr[T] {
    def in[U <: Universe with Singleton](otherMirror: scala.reflect.api.Mirror[U]): U # Expr[T] = {
      val otherMirror1 = otherMirror.asInstanceOf[scala.reflect.api.Mirror[otherMirror.universe.type]]
      val tag1 = (implicitly[WeakTypeTag[T]] in otherMirror).asInstanceOf[otherMirror.universe.WeakTypeTag[T]]
      otherMirror.universe.Expr[T](otherMirror1, treec)(tag1)
    }

    lazy val tree: Tree = treec(mirror)
    lazy val staticType: Type = implicitly[WeakTypeTag[T]].tpe
    def actualType: Type = tree.tpe

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

    @throws(classOf[ObjectStreamException])
    private def writeReplace(): AnyRef = new SerializedExpr(treec, implicitly[WeakTypeTag[T]].in(ru.rootMirror))
  }
}

@SerialVersionUID(1L)
private[scala] class SerializedExpr(var treec: TreeCreator, var tag: ru.WeakTypeTag[_]) extends Serializable {
  import scala.reflect.runtime.universe.{Expr, runtimeMirror}

  @throws(classOf[ObjectStreamException])
  private def readResolve(): AnyRef = {
    val loader: ClassLoader = try {
      Thread.currentThread().getContextClassLoader()
    } catch {
      case se: SecurityException => null
    }
    val m = runtimeMirror(loader)
    Expr(m, treec)(tag.in(m))
  }
}
