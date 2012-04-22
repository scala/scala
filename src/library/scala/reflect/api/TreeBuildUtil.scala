package scala.reflect
package api

trait TreeBuildUtil { self: Universe =>

  /** The symbol corresponding to the globally accessible class with the given fully qualified name `fullName`.
   *  Unlike `staticClassIfDefined`, throws `MissingRequirementError` is requested class cannot be found.
   */
  def staticClass(fullName: String): Symbol

  /** The symbol corresponding to the globally accessible class with the given fully qualified name `fullName`.
   *  Unlike `staticClass`, doesn't throw `MissingRequirementError` (returns NoSymbol) is requested class cannot be found.
   */
  def staticClassIfDefined(fullName: String): Symbol

  /** The symbol corresponding to the globally accessible object with the given fully qualified name `fullName`.
   *  Unlike `staticModuleIfDefined`, throws `MissingRequirementError` is requested object cannot be found.
   */
  def staticModule(fullName: String): Symbol

  /** The symbol corresponding to the globally accessible object with the given fully qualified name `fullName`.
   *  Unlike `staticModule`, doesn't throw `MissingRequirementError` (returns NoSymbol) is requested object cannot be found.
   */
  def staticModuleIfDefined(fullName: String): Symbol

  /** The this-ptype of the globally accessible object with the
   *  given fully qualified name `fullName`.
   */
  def thisModuleType(fullName: String): Type

  /** Selects type symbol with given simple name `name` from the defined members of `owner`.
   *  Unlike `selectTypeIfDefined`, throws `MissingRequirementError` is requested type symbol cannot be found.
   */
  def selectType(owner: Symbol, name: String): Symbol

  /** Selects type symbol with given simple name `name` from the defined members of `owner`.
   *  Unlike `selectType`, doesn't throw `MissingRequirementError` (returns NoSymbol) is requested type symbol cannot be found.
   */
  def selectTypeIfDefined(owner: Symbol, name: String): Symbol

  /** Selects term symbol with given name and type from the defined members of prefix type
   *  Unlike `selectTermIfDefined`, throws `MissingRequirementError` is requested term symbol cannot be found.
   */
  def selectTerm(owner: Symbol, name: String): Symbol

  /** Selects term symbol with given name and type from the defined members of prefix type
   *  Unlike `selectTerm`, doesn't throw `MissingRequirementError` (returns NoSymbol) is requested term symbol cannot be found.
   */
  def selectTermIfDefined(owner: Symbol, name: String): Symbol

  /** Selects overloaded method symbol with given name and index
   *  Unlike `selectOverloadedMethodIfDefined`, throws `MissingRequirementError` is requested overloaded method cannot be found.
   */
  def selectOverloadedMethod(owner: Symbol, name: String, index: Int): Symbol

  /** Selects overloaded method symbol with given name and index
   *  Unlike `selectOverloadedMethod`, doesn't throw `MissingRequirementError` (returns NoSymbol) is requested overloaded method cannot be found.
   */
  def selectOverloadedMethodIfDefined(owner: Symbol, name: String, index: Int): Symbol

  /** Create a fresh free term symbol.
   *  @param   name   the name of the free variable
   *  @param   info   the type signature of the free variable
   *  @param   value  the value of the free variable at runtime
   *  @param   flags  (optional) flags of the free variable
   *  @param   origin debug information that tells where this symbol comes from
   */
  def newFreeTerm(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null): Symbol

  /** Create a fresh free non-existential type symbol.
   *  @param   name   the name of the free variable
   *  @param   info   the type signature of the free variable
   *  @param   value  a type tag that captures the value of the free variable
   *                  is completely phantom, since the captured type cannot be propagated to the runtime
   *                  if it could be, we wouldn't be creating a free type to begin with
   *                  the only usage for it is preserving the captured symbol for compile-time analysis
   *  @param   flags  (optional) flags of the free variable
   *  @param   origin debug information that tells where this symbol comes from
   */
  def newFreeType(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null): Symbol

  /** Create a fresh free existential type symbol.
   *  @param   name   the name of the free variable
   *  @param   info   the type signature of the free variable
   *  @param   value  a type tag that captures the value of the free variable
   *                  is completely phantom, since the captured type cannot be propagated to the runtime
   *                  if it could be, we wouldn't be creating a free type to begin with
   *                  the only usage for it is preserving the captured symbol for compile-time analysis
   *  @param   flags  (optional) flags of the free variable
   *  @param   origin (optional) debug information that tells where this symbol comes from
   */
  def newFreeExistential(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null): Symbol

  /** Create a Modiiers structure given internal flags, qualifier, annotations */
  def modifiersFromInternalFlags(flags: Long, privateWithin: Name, annotations: List[Tree]): Modifiers

  val gen: TreeGen { val global: TreeBuildUtil.this.type }

  type TreeGen <: AbsTreeGen
}

// [Eugene to Paul] we need to expose some of the functionality provided by TreeGen
// I added some stuff that was necessary for typetag materialization macros
// but we should think it over and pick other generally useful stuff
// same goes for tree traversers/transformers, type maps, etc
// and once we expose all that, there's another question: how do we stay in sync?
trait AbsTreeGen {
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
