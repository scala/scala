package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * A slice of [[scala.reflect.macros.blackbox.Context the Scala macros context]] that
 *  exposes functions to save reflection artifacts for runtime.
 */
trait Reifiers {
  self: blackbox.Context =>

  /** Given a tree, generate a tree that when compiled and executed produces the original tree.
   *  For more information and examples see the documentation for `Universe.reify`.
   *
   *  The produced tree will be bound to the specified `universe` and `mirror`.
   *  Possible values for `universe` include `universe.internal.gen.mkRuntimeUniverseRef`.
   *  Possible values for `mirror` include `EmptyTree` (in that case the reifier will automatically pick an appropriate mirror).
   *
   *  This function is deeply connected to `Universe.reify`, a macro that reifies arbitrary expressions into runtime trees.
   *  They do very similar things (`Universe.reify` calls `Context.reifyTree` to implement itself), but they operate on different metalevels (see below).
   *
   *  Let's study the differences between `Context.reifyTree` and `Universe.reify` on an example of using them inside a `fooMacro` macro:
   *
   *    * Since reify itself is a macro, it will be executed when fooMacro is being compiled (metalevel -1)
   *      and will produce a tree that when evaluated during macro expansion of fooMacro (metalevel 0) will recreate the input tree.
   *
   *      This provides a facility analogous to quasi-quoting. Writing "reify{ expr }" will generate an AST that represents expr.
   *      Afterwards this AST (or its parts) can be used to construct the return value of fooMacro.
   *
   *    * reifyTree is evaluated during macro expansion (metalevel 0)
   *      and will produce a tree that when evaluated during the runtime of the program (metalevel 1) will recreate the input tree.
   *
   *      This provides a way to retain certain trees from macro expansion time to be inspected later, in the runtime.
   *      For example, DSL authors may find it useful to capture DSL snippets into ASTs that are then processed at runtime in a domain-specific way.
   *
   *  Also note the difference between universes of the runtime trees produced by two reifies:
   *
   *    * The result of compiling and running the result of reify will be bound to the Universe that called reify.
   *      This is possible because it's a macro, so it can generate whatever code it wishes.
   *
   *    * The result of compiling and running the result of reifyTree will be the `prefix` that needs to be passed explicitly.
   *      This happens because the Universe of the evaluated result is from a different metalevel than the Context the called reify.
   *
   *  Typical usage of this function is to retain some of the trees received/created by a macro
   *  into the form that can be inspected (via pattern matching) or compiled/run (by a reflective ToolBox) during the runtime.
   */
  def reifyTree(universe: Tree, mirror: Tree, tree: Tree): Tree

  /** Given a type, generate a tree that when compiled and executed produces the original type.
   *  The produced tree will be bound to the specified `universe` and `mirror`.
   *  For more information and examples see the documentation for `Context.reifyTree` and `Universe.reify`.
   */
  def reifyType(universe: Tree, mirror: Tree, tpe: Type, concrete: Boolean = false): Tree

  /** Given a type, generate a tree that when compiled and executed produces the runtime class of the original type.
   *  If `concrete` is true, then this function will bail on types, who refer to abstract types (like `ClassTag` does).
   */
  def reifyRuntimeClass(tpe: Type, concrete: Boolean = true): Tree

  /** Given a type, generate a tree that when compiled and executed produces the runtime class of the enclosing class or module.
   *  Returns `EmptyTree` if there does not exist an enclosing class or module.
   */
  def reifyEnclosingRuntimeClass: Tree

  /** Undoes reification of a tree.
   *
   *  This reversion doesn't simply restore the original tree (that would lose the context of reification),
   *  but does something more involved that conforms to the following laws:
   *
   *    1) unreifyTree(reifyTree(tree)) != tree                                 // unreified tree is tree + saved context
   *                                                                            // in current implementation, the result of unreify is opaque
   *                                                                            // i.e. there's no possibility to inspect underlying tree/context
   *
   *    2) reifyTree(unreifyTree(reifyTree(tree))) == reifyTree(tree)           // the result of reifying a tree in its original context equals to
   *                                                                            // the result of reifying a tree along with its saved context
   *
   *    3) compileAndEval(unreifyTree(reifyTree(tree))) ~ compileAndEval(tree)  // at runtime original and unreified trees are behaviorally equivalent
   */
  def unreifyTree(tree: Tree): Tree
}

// made these guys non path-dependent, otherwise exception handling quickly becomes a mess

/** Indicates an expected error during one of the `reifyXXX` methods in [[scala.reflect.macros.Reifiers]].
 *  Such errors represent one of the standard ways for reification to go wrong, e.g.
 *  an attempt to create a `TypeTag` from a weak type.
 */
case class ReificationException(pos: scala.reflect.api.Position, msg: String) extends Exception(msg)

/** Indicates an unexpected expected error during one of the `reifyXXX` methods in [[scala.reflect.macros.Reifiers]].
 *  Such errors wrap random crashes in reification logic and are distinguished from expected [[scala.reflect.macros.ReificationException]]s
 *  so that the latter can be reported as compilation errors, while the former manifest themselves as compiler crashes.
 */
case class UnexpectedReificationException(pos: scala.reflect.api.Position, msg: String, cause: Throwable = null) extends Exception(msg, cause)
