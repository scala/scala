package scala.reflect
package internal

import Flags._

trait TreeBuildUtil extends api.TreeBuildUtil { self: SymbolTable =>

  // ``staticClass'' and ``staticModule'' rely on ClassLoaders
  // which are implementation-specific for different Universes

  def staticClassIfDefined(fullName: String): Symbol =
    try staticClass(fullName)
    catch { case _: MissingRequirementError => NoSymbol }

  def staticModuleIfDefined(fullName: String): Symbol =
    try staticModule(fullName)
    catch { case _: MissingRequirementError => NoSymbol }

  def thisModuleType(fullname: String) = staticModule(fullname).moduleClass.thisType

  def selectType(owner: Symbol, name: String): Symbol =
    owner.info.decl(newTypeName(name)) orElse {
      MissingRequirementError.notFound("type %s in %s".format(name, owner.fullName))
    }

  def selectTypeIfDefined(owner: Symbol, name: String): Symbol =
    try selectType(owner, name)
    catch { case _: MissingRequirementError => NoSymbol }

//  try getModule(fullname.toTermName)
//  catch { case _: MissingRequirementError => NoSymbol }

  def selectTerm(owner: Symbol, name: String): Symbol = {
    val sym = owner.info.decl(newTermName(name))
    val result =
      if (sym.isOverloaded) sym suchThat (!_.isMethod)
      else sym
    result orElse {
      MissingRequirementError.notFound("term %s in %s".format(name, owner.fullName))
    }
  }

  def selectTermIfDefined(owner: Symbol, name: String): Symbol =
    try selectTerm(owner, name)
    catch { case _: MissingRequirementError => NoSymbol }

  def selectOverloadedMethod(owner: Symbol, name: String, index: Int): Symbol =
    owner.info.decl(newTermName(name)).alternatives(index) orElse {
      MissingRequirementError.notFound("overloaded method %s #%d in %s".format(name, index, owner.fullName))
    }

  def selectOverloadedMethodIfDefined(owner: Symbol, name: String, index: Int): Symbol =
    try selectOverloadedMethod(owner, name, index)
    catch { case _: MissingRequirementError => NoSymbol }

  def newFreeTerm(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null) = newFreeTermSymbol(newTermName(name), info, value, flags, origin)

  def newFreeType(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null) = newFreeTypeSymbol(newTypeName(name), info, value, (if (flags == 0L) PARAM else flags) | DEFERRED, origin)

  def newFreeExistential(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null) = newFreeTypeSymbol(newTypeName(name), info, value, (if (flags == 0L) EXISTENTIAL else flags) | DEFERRED, origin)

  def modifiersFromInternalFlags(flags: Long, privateWithin: Name, annotations: List[Tree]): Modifiers =
    Modifiers(flags, privateWithin, annotations)

  val gen: TreeGen { val global: TreeBuildUtil.this.type }
}