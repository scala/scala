package scala.reflect
package runtime

trait TreeBuildUtil extends Universe with api.TreeBuildUtil {
  /** A comment to the effect of why initialize was added to all these
   *  would be appreciated.  (We may as well start somewhere.)
   */
  def staticClass(fullname: String)    = definitions.getRequiredClass(fullname).initialize
  def staticModule(fullname: String)   = definitions.getRequiredModule(fullname).initialize
  def thisModuleType(fullname: String) = staticModule(fullname).moduleClass.initialize.thisType

 /** Selects type symbol with given name from the defined members of prefix type
   */
  def selectType(owner: Symbol, name: String): Symbol =
    owner.info.decl(newTypeName(name))

  /** Selects term symbol with given name and type from the defined members of prefix type
   *  @pre   The prefix type
   *  @name  The name of the selected member
   */
  def selectTerm(owner: Symbol, name: String): Symbol = {
    val sym = owner.info.decl(newTermName(name))
    if (sym.isOverloaded) sym suchThat (!_.isMethod)
    else sym
  }

  def selectOverloadedMethod(owner: Symbol, name: String, index: Int): Symbol =
    owner.info.decl(newTermName(name)).alternatives(index)

  def selectParam(owner: Symbol, idx: Int): Symbol = {
    def selectInList(params: List[Symbol], idx: Int, fallback: Type): Symbol = {
      if (params.isEmpty) selectIn(fallback, idx)
      else if (idx == 0) params.head
      else selectInList(params.tail, idx - 1, fallback)
    }
    def selectIn(tpe: Type, idx: Int): Symbol = tpe match {
      case PolyType(tparams, res) => selectInList(tparams, idx, res)
      case MethodType(params, res) => selectInList(params, idx, res)
      case _ => NoSymbol
    }
    selectIn(owner.info, idx)
  }

  def newFreeVar(name: String, info: Type, value: Any) = newFreeVar(newTermName(name), info, value)

  def modifiersFromInternalFlags(flags: Long, privateWithin: Name, annotations: List[Tree]): Modifiers =
    Modifiers(flags, privateWithin, annotations)

}