package scala.reflect
package makro

// [Eugene] I added some stuff that was necessary for typetag materialization macros
// but we should think it over and pick other generally useful stuff
// same goes for tree traversers/transformers, type maps, etc
// and once we expose all that, there's another question: how do we stay in sync?
abstract class TreeBuilder {
  val global: Universe

  import global._
  import definitions._

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
  def mkAttributedRef(pre: Type, sym: Symbol): Tree

  /** Builds a typed reference to given symbol. */
  def mkAttributedRef(sym: Symbol): Tree

  /** Builds a typed This reference to given symbol. */
  def mkAttributedThis(sym: Symbol): Tree

  /** Builds a typed Ident with an underlying symbol. */
  def mkAttributedIdent(sym: Symbol): Tree

  /** Builds a typed Select with an underlying symbol. */
  def mkAttributedSelect(qual: Tree, sym: Symbol): Tree

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
}
