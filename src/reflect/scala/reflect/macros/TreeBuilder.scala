package scala.reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A helper available in [[scala.reflect.macros.Universe]] that defines shorthands for the
 *  most common tree-creating functions.
 */
abstract class TreeBuilder {
  val global: Universe

  import global._

  /** Builds a reference to value whose type is given stable prefix.
   *  The type must be suitable for this.  For example, it
   *  must not be a TypeRef pointing to an abstract type variable.
   */
  def mkAttributedQualifier(tpe: Type): Tree

  /** Builds a reference to value whose type is given stable prefix.
   *  If the type is unsuitable, e.g. it is a TypeRef for an
   *  abstract type variable, then an Ident will be made using
   *  termSym as the Ident's symbol.  In that case, termSym must
   *  not be NoSymbol.
   */
  def mkAttributedQualifier(tpe: Type, termSym: Symbol): Tree

  /** Builds a typed reference to given symbol with given stable prefix. */
  def mkAttributedRef(pre: Type, sym: Symbol): RefTree

  /** Builds a typed reference to given symbol. */
  def mkAttributedRef(sym: Symbol): RefTree

  /** Builds an untyped reference to given symbol. Requires the symbol to be static. */
  def mkUnattributedRef(sym: Symbol): RefTree

  /** Builds an untyped reference to symbol with given name. Requires the symbol to be static. */
  def mkUnattributedRef(fullName: Name): RefTree

  /** Builds a typed This reference to given symbol. */
  def mkAttributedThis(sym: Symbol): This

  /** Builds a typed Ident with an underlying symbol. */
  def mkAttributedIdent(sym: Symbol): RefTree

  /** Builds a typed Select with an underlying symbol. */
  def mkAttributedSelect(qual: Tree, sym: Symbol): RefTree

  /** A creator for method calls, e.g. fn[T1, T2, ...](v1, v2, ...)
   *  There are a number of variations.
   *
   *  @param    receiver    symbol of the method receiver
   *  @param    methodName  name of the method to call
   *  @param    targs       type arguments (if Nil, no TypeApply node will be generated)
   *  @param    args        value arguments
   *  @return               the newly created trees.
   */
  def mkMethodCall(receiver: Symbol, methodName: Name, targs: List[Type], args: List[Tree]): Tree

  def mkMethodCall(method: Symbol, targs: List[Type], args: List[Tree]): Tree

  def mkMethodCall(method: Symbol, args: List[Tree]): Tree

  def mkMethodCall(target: Tree, args: List[Tree]): Tree

  def mkMethodCall(receiver: Symbol, methodName: Name, args: List[Tree]): Tree

  def mkMethodCall(receiver: Tree, method: Symbol, targs: List[Type], args: List[Tree]): Tree

  def mkMethodCall(target: Tree, targs: List[Type], args: List[Tree]): Tree

  def mkNullaryCall(method: Symbol, targs: List[Type]): Tree

  /** A tree that refers to the runtime reflexive universe, ``scala.reflect.runtime.universe''. */
  def mkRuntimeUniverseRef: Tree
}
