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

package scala.reflect.macros
package contexts

trait Aliases {
  self: Context =>

  override type Symbol = universe.Symbol
  override type Type = universe.Type
  override type Name = universe.Name
  override type TermName = universe.TermName
  override type TypeName = universe.TypeName
  override type Tree = universe.Tree
  override type Position = universe.Position
  override type Scope = universe.Scope
  override type Modifiers = universe.Modifiers

  override type Expr[+T] = universe.Expr[T]
  override val Expr = universe.Expr
  def Expr[T: WeakTypeTag](tree: Tree): Expr[T] = universe.Expr[T](mirror, universe.FixedMirrorTreeCreator(mirror, tree))

  override type WeakTypeTag[T] = universe.WeakTypeTag[T]
  override type TypeTag[T] = universe.TypeTag[T]
  override val WeakTypeTag = universe.WeakTypeTag
  override val TypeTag = universe.TypeTag
  def WeakTypeTag[T](tpe: Type): WeakTypeTag[T] = universe.WeakTypeTag[T](mirror, universe.FixedMirrorTypeCreator(mirror, tpe))
  def TypeTag[T](tpe: Type): TypeTag[T] = universe.TypeTag[T](mirror, universe.FixedMirrorTypeCreator(mirror, tpe))
  override def weakTypeTag[T](implicit attag: WeakTypeTag[T]) = attag
  override def typeTag[T](implicit ttag: TypeTag[T]) = ttag
  override def weakTypeOf[T](implicit attag: WeakTypeTag[T]): Type = attag.tpe
  override def typeOf[T](implicit ttag: TypeTag[T]): Type = ttag.tpe

  implicit class RichOpenImplicit(oi: universe.analyzer.OpenImplicit) {
    def toImplicitCandidate = ImplicitCandidate(oi.info.pre, oi.info.sym, oi.pt, oi.tree)
  }
}
