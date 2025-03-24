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

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * The base class for all mirrors.
 *
 * See [[scala.reflect.api.Mirrors]] or [[https://docs.scala-lang.org/overviews/reflection/overview.html Reflection Guide]]
 * for a complete overview of `Mirror`s.
 *
 * @tparam U the type of the universe this mirror belongs to.
 *  @group ReflectionAPI
 */
// Note: Unlike most Scala reflection artifact classes, `Mirror` is not defined as an inner class,
// so that it can be referenced from outside. For example, [[scala.reflect.api.TypeCreator]] and [[scala.reflect.api.TreeCreator]]
// reference `Mirror` and also need to be defined outside the cake as they are used by type tags, which can be migrated between
// different universes and consequently cannot be bound to a fixed one.
abstract class Mirror[U <: Universe with Singleton] {
  /** The universe this mirror belongs to.
   *  @group Mirror
   */
  val universe: U

  /** The class symbol of the `_root_` package
   *  @group Mirror
   */
  def RootClass: U#ClassSymbol

  /** The module symbol of the `_root_` package
   *  @group Mirror
   */
  def RootPackage: U#ModuleSymbol

  /** The module class symbol of the default (unnamed) package
   *  @group Mirror
   */
  def EmptyPackageClass: U#ClassSymbol

  /** The module symbol of the default (unnamed) package
   *  @group Mirror
   */
  def EmptyPackage: U#ModuleSymbol

  /** The symbol corresponding to the globally accessible class with the
   *  given fully qualified name `fullName`.
   *
   *  If the name points to a type alias, it's recursively dealiased and its target is returned.
   *  If you need a symbol that corresponds to the type alias itself, load it directly from the package class:
   *
   *    scala> cm.staticClass("scala.List")
   *    res0: scala.reflect.runtime.universe.ClassSymbol = class List
   *
   *    scala> res0.fullName
   *    res1: String = scala.collection.immutable.List
   *
   *    scala> cm.staticPackage("scala")
   *    res2: scala.reflect.runtime.universe.ModuleSymbol = package scala
   *
   *    scala> res2.moduleClass.info member TypeName("List")
   *    res3: scala.reflect.runtime.universe.Symbol = type List
   *
   *    scala> res3.fullName
   *    res4: String = scala.List
   *
   *  To be consistent with Scala name resolution rules, in case of ambiguity between
   *  a package and an object, the object is never been considered.
   *
   *  For example for the following code:
   *
   *    package foo {
   *      class B
   *    }
   *
   *    object foo {
   *      class A
   *      class B
   *    }
   *
   *  staticClass("foo.B") will resolve to the symbol corresponding to the class B declared in the package foo, and
   *  staticClass("foo.A") will throw a ScalaReflectionException.
   *
   *  In the example above, to load a symbol that corresponds to the class B declared in the object foo,
   *  use staticModule("foo") to load the module symbol and then navigate info.members of its moduleClass.
   *  @group Mirror
   */
  def staticClass(fullName: String): U#ClassSymbol

  /** The symbol corresponding to the globally accessible object with the
   *  given fully qualified name `fullName`.
   *
   *  To be consistent with Scala name resolution rules, in case of ambiguity between
   *  a package and an object, the object is never been considered.
   *
   *  For example for the following code:
   *
   *    package foo {
   *      object B
   *    }
   *
   *    object foo {
   *      object A
   *      object B
   *    }
   *
   *  staticModule("foo.B") will resolve to the symbol corresponding to the object B declared in the package foo, and
   *  staticModule("foo.A") will throw a ScalaReflectionException
   *
   *  In the example above, to load a symbol that corresponds to the object B declared in the object foo,
   *  use staticModule("foo") to load the module symbol and then navigate info.members of its moduleClass.
   *  @group Mirror
   */
  def staticModule(fullName: String): U#ModuleSymbol

  /** The symbol corresponding to a package with the
   *  given fully qualified name `fullName`.
   *  @group Mirror
   */
  def staticPackage(fullName: String): U#ModuleSymbol

  /**
   * Shortcut for `implicitly[WeakTypeTag[T]].tpe`
   * @group TypeTags
   */
  def weakTypeOf[T: universe.WeakTypeTag]: U#Type = universe.weakTypeTag[T].in(this).tpe

  /**
   * Shortcut for `implicitly[TypeTag[T]].tpe`
   * @group TypeTags
   */
  def typeOf[T: universe.TypeTag]: U#Type = universe.typeTag[T].in(this).tpe

  /**
   * Type symbol of `x` as derived from a type tag.
   * @group TypeTags
   */
  def symbolOf[T: universe.WeakTypeTag]: U#TypeSymbol
}
