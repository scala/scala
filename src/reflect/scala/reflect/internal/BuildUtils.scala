package scala.reflect
package internal

import Flags._

trait BuildUtils extends base.BuildUtils { self: SymbolTable =>

  class BuildImpl extends BuildBase {

    def selectType(owner: Symbol, name: String): TypeSymbol = {
      val result = owner.info.decl(newTypeName(name))
      if (result ne NoSymbol) result.asTypeSymbol
      else MissingRequirementError.notFound("type %s in %s".format(name, owner.fullName))
    }

    def selectTerm(owner: Symbol, name: String): TermSymbol = {
      val sym = owner.info.decl(newTermName(name))
      val result =
        if (sym.isOverloaded) sym.suchThat(!_.isMethod)
        else sym
      if (result ne NoSymbol) result.asTermSymbol
      else MissingRequirementError.notFound("term %s in %s".format(name, owner.fullName))
    }

    def selectOverloadedMethod(owner: Symbol, name: String, index: Int): MethodSymbol = {
      val result = owner.info.decl(newTermName(name)).alternatives(index)
      if (result ne NoSymbol) result.asMethodSymbol
      else MissingRequirementError.notFound("overloaded method %s #%d in %s".format(name, index, owner.fullName))
    }

    def newFreeTerm(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
      newFreeTermSymbol(newTermName(name), info, value, flags, origin)

    def newFreeType(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
      newFreeTypeSymbol(newTypeName(name), info, value, (if (flags == 0L) PARAM else flags) | DEFERRED, origin)

    def newFreeExistential(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
      newFreeTypeSymbol(newTypeName(name), info, value, (if (flags == 0L) EXISTENTIAL else flags) | DEFERRED, origin)

    def newNestedSymbol(owner: Symbol, name: Name, pos: Position, flags: Long, isClass: Boolean): Symbol =
      owner.newNestedSymbol(name, pos, flags, isClass)

    def setAnnotations[S <: Symbol](sym: S, annots: List[AnnotationInfo]): S =
      sym.setAnnotations(annots)

    def setTypeSignature[S <: Symbol](sym: S, tpe: Type): S =
      sym.setTypeSignature(tpe)

    def flagsFromBits(bits: Long): FlagSet = bits

    def emptyValDef: ValDef = self.emptyValDef

    def This(sym: Symbol): Tree = self.This(sym)

    def Select(qualifier: Tree, sym: Symbol): Select = self.Select(qualifier, sym)

    def Ident(sym: Symbol): Ident = self.Ident(sym)

    def TypeTree(tp: Type): TypeTree = self.TypeTree(tp)

    def thisPrefix(sym: Symbol): Type = sym.thisPrefix

    def setType[T <: Tree](tree: T, tpe: Type): T = { tree.setType(tpe); tree }

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T = { tree.setSymbol(sym); tree }
  }

  val build: BuildBase = new BuildImpl
}
