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
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.blackbox.Context the Scala macros context]] that exposes
 *  enclosing trees (method, class, compilation unit and currently compiled macro application),
 *  the enclosing position of the macro expansion, as well as macros and implicits
 *  that are currently in-flight.
 *
 *  Starting from Scala 2.11.0, the APIs to get the trees enclosing by the current macro application are deprecated,
 *  and the reasons for that are two-fold. Firstly, we would like to move towards the philosophy of locally-expanded macros,
 *  as it has proven to be important for understanding of code. Secondly, within the current architecture of scalac,
 *  we are unable to have c.enclosingTree-style APIs working robustly. Required changes to the typechecker would greatly
 *  exceed the effort that we would like to expend on this feature given the existence of more pressing concerns at the moment.
 *  This is somewhat aligned with the overall evolution of macros during the 2.11 development cycle, where we played with
 *  `c.introduceTopLevel` and `c.introduceMember`, but at the end of the day decided to reject them.
 *
 *  If you're relying on the now deprecated APIs, consider using the new [[Internals.ContextInternalApi.enclosingOwner]] method that can be used to obtain
 *  the names of enclosing definitions. Alternatively try reformulating your macros in terms of completely local expansion
 *  and/or joining a discussion of a somewhat related potential language feature at [[https://groups.google.com/forum/#!topic/scala-debate/f4CLmYShX6Q]].
 *  We also welcome questions and suggestions on our mailing lists, where we would be happy to further discuss this matter.
 */
trait Enclosures {
  self: blackbox.Context =>

  /** The tree that undergoes macro expansion.
   *  Can be useful to get an offset or a range position of the entire tree being processed.
   */
  def macroApplication: Tree

  /** Contexts that represent macros in-flight, including the current one. Very much like a stack trace, but for macros only.
   *  Can be useful for interoperating with other macros and for imposing compiler-friendly limits on macro expansion.
   *
   *  Is also priceless for emitting sane error messages for macros that are called by other macros on synthetic (i.e. position-less) trees.
   *  In that dire case navigate the `enclosingMacros` stack, and it will most likely contain at least one macro with a position-ful macro application.
   *  See `enclosingPosition` for a default implementation of this logic.
   *
   *  Unlike `openMacros`, this is a val, which means that it gets initialized when the context is created
   *  and always stays the same regardless of whatever happens during macro expansion.
   */
  def enclosingMacros: List[blackbox.Context]

  /** Tries to guess a position for the enclosing application.
   *  But that is simple, right? Just dereference `pos` of `macroApplication`? Not really.
   *  If we're in a synthetic macro expansion (no positions), we must do our best to infer the position of something that triggered this expansion.
   *  Surprisingly, quite often we can do this by navigation the `enclosingMacros` stack.
   */
  def enclosingPosition: Position

  /** Tree that corresponds to the enclosing method, or EmptyTree if not applicable.
   *  @see [[scala.reflect.macros.Enclosures]]
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  def enclosingMethod: Tree

  /** Tree that corresponds to the enclosing class, or EmptyTree if not applicable.
   *  @see [[scala.reflect.macros.Enclosures]]
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  def enclosingClass: Tree

  /** Tree that corresponds to the enclosing DefDef tree.
   *  Throws `EnclosureException` if there's no such enclosing tree.
   *  @see [[scala.reflect.macros.Enclosures]]
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  def enclosingDef: universe.DefDef

  /** Tree that corresponds to the enclosing Template tree.
   *  Throws `EnclosureException` if there's no such enclosing tree.
   *  @see [[scala.reflect.macros.Enclosures]]
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  def enclosingTemplate: universe.Template

  /** Tree that corresponds to the enclosing ImplDef tree (i.e. either ClassDef or ModuleDef).
   *  Throws `EnclosureException` if there's no such enclosing tree.
   *  @see [[scala.reflect.macros.Enclosures]]
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  def enclosingImpl: universe.ImplDef

  /** Tree that corresponds to the enclosing PackageDef tree.
   *  Throws `EnclosureException` if there's no such enclosing tree.
   *  @see [[scala.reflect.macros.Enclosures]]
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  def enclosingPackage: universe.PackageDef

  /** Compilation unit that contains this macro application.
   *  @see [[scala.reflect.macros.Enclosures]]
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  def enclosingUnit: CompilationUnit

  /** Compilation run that contains this macro application.
   *  @see [[scala.reflect.macros.Enclosures]]
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  def enclosingRun: Run

  /** Indicates than one of the enclosure methods failed to find a tree
   *  of required type among enclosing trees.
   *  @see [[scala.reflect.macros.Enclosures]]
   */
  @deprecated("c.enclosingTree-style APIs are now deprecated; consult the scaladoc for more information", "2.11.0")
  case class EnclosureException(expected: Class[_], enclosingTrees: List[Tree])
  extends Exception(s"Couldn't find a tree of type $expected among enclosing trees $enclosingTrees")
}
