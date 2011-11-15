package scala.reflect.api

trait TreeBuildUtil extends Universe {

  /** The symbol corresponding to the globally accessible class with the
   *  given fully qualified name `fullname`.
   */
  def staticClass(fullname: String): Symbol

  /** The symbol corresponding to the globally accessible object with the
   *  given fully qualified name `fullname`.
   */
  def staticModule(fullname: String): Symbol

  /** The this-ptype of the globally accessible object with the
   *  given fully qualified name `fullname`.
   */
  def thisModuleType(fullname: String): Type

  /** Selects type symbol with given simple name `name` from the defined members of `owner`.
   */
  def selectType(owner: Symbol, name: String): Symbol

  /** Selects term symbol with given name and type from the defined members of prefix type
   *  @pre   The prefix type
   *  @name  The name of the selected member
   */
  def selectTerm(owner: Symbol, name: String): Symbol

  def selectOverloadedMethod(owner: Symbol, name: String, index: Int): Symbol

  def selectParam(owner: Symbol, idx: Int): Symbol

  def newScopeWith(decls: Symbol*): Scope

  /** Create a fresh free variable symbol.
   *  @param   name   the name of the free variable
   *  @param   tsig   the type signature of the free variable
   *  @param   value  the value of the free variable at runtime
   */
  def freeVar(name: String, tsig: Type, value: Any): Symbol

  /** Create a Modiiers structure given internal flags, qualifier, annotations */
  def modifiersFromInternalFlags(flags: Long, privateWithin: Name, annotations: List[Tree]): Modifiers

}