package scala.tools.nsc.tasty.bridge

import scala.reflect.internal.SymbolTable

trait TastyKernel { self =>

  val symbolTable: SymbolTable

  final implicit val thisUniverse: self.type = self
  final implicit val symbolTablePrecise: self.symbolTable.type = self.symbolTable

  type Context <: AnyRef

  type FlagSet = symbolTable.FlagSet
  def  Flag    = symbolTable.Flag
  def  NoFlags = symbolTable.NoFlags

  def reporter = symbolTable.reporter

  type Postion = symbolTable.Position
  def NoPosition: Postion = symbolTable.NoPosition

  private[bridge] def settings = symbolTable.settings
  private[bridge] def phase: Phase = symbolTable.phase

  type Type = symbolTable.Type
  type ClassInfoType = symbolTable.ClassInfoType
  type ExistentialType = symbolTable.ExistentialType
  type MethodType = symbolTable.MethodType
  type PolyType = symbolTable.PolyType

  type NullaryMethodType = symbolTable.NullaryMethodType

  object NullaryMethodType {
    def apply(tpe: Type): NullaryMethodType = symbolTable.NullaryMethodType(tpe)
  }

  type ConstantType = symbolTable.ConstantType
  def isConstantType(tpe: Type): Boolean = tpe.isInstanceOf[symbolTable.ConstantType]
  def ConstantType = symbolTable.ConstantType

  def ErrorType = symbolTable.ErrorType

  type ThisType = symbolTable.ThisType

  def NoPrefix = symbolTable.NoPrefix

  type TypeBounds = symbolTable.TypeBounds
  object TypeBounds {
    def empty = symbolTable.TypeBounds.empty
    def unapply(tpe: TypeBounds): Option[(Type, Type)] = symbolTable.TypeBounds.unapply(tpe)
  }

  object defn {

    def NothingTpe = symbolTable.definitions.NothingTpe
    def AnyRefTpe = symbolTable.definitions.AnyRefTpe

    def ByNameParamClass: ClassSymbol = symbolTable.definitions.ByNameParamClass

    def ObjectClass: ClassSymbol = symbolTable.definitions.ObjectClass
    def AnyValClass: ClassSymbol = symbolTable.definitions.AnyValClass

  }

  object nme {
    def EMPTY: TermName = symbolTable.nme.EMPTY
    def CONSTRUCTOR: TermName = symbolTable.nme.CONSTRUCTOR
    def ROOT: TermName = symbolTable.nme.ROOT
    def ROOTPKG: TermName = symbolTable.nme.ROOTPKG
    def EMPTY_PACKAGE_NAME: TermName = symbolTable.nme.EMPTY_PACKAGE_NAME
  }

  object termNames {
    def EMPTY: TermName = symbolTable.termNames.EMPTY
  }

  type TypeRef = symbolTable.TypeRef

  private[bridge] type LazyType = symbolTable.LazyType
  private[bridge] type FlagAgnosticCompleter = symbolTable.FlagAgnosticCompleter

  def NoSymbol = symbolTable.NoSymbol

  type Scope = symbolTable.Scope
  def EmptyScope: Scope = symbolTable.EmptyScope

  type Symbol = symbolTable.Symbol
  type TermSymbol = symbolTable.TermSymbol
  type ModuleSymbol = symbolTable.ModuleSymbol
  type ClassSymbol = symbolTable.ClassSymbol
  type FreeTypeSymbol = symbolTable.FreeTypeSymbol

  type Constant = symbolTable.Constant
  def  Constant = symbolTable.Constant

  type Mirror = symbolTable.Mirror

  type Name   = symbolTable.Name
  type TermName = symbolTable.TermName
  type TypeName = symbolTable.TypeName

  def showRaw(flags: Long): String = symbolTable.showRaw(flags)
  def showRaw(any: Any): String = symbolTable.showRaw(any)

  def mkTermName(str: String): TermName = {
    import symbolTable._
    str
  }

  type Tree = symbolTable.Tree

  type RefTree = symbolTable.RefTree
  def RefTree = symbolTable.RefTree

  type TypeTree = symbolTable.TypeTree
  def TypeTree(tp: Type): TypeTree = symbolTable.TypeTree(tp)

  type This = symbolTable.This

  type Literal = symbolTable.Literal
  def  Literal = symbolTable.Literal

  type Ident = symbolTable.Ident
  def Ident(name: Name) = symbolTable.Ident.apply(name)

  def New(tpt: Tree) = symbolTable.New.apply(tpt)

  type Select = symbolTable.Select
  def Select(qual: Tree, name: Name) = symbolTable.Select.apply(qual, name)

  type Annotation = symbolTable.Annotation
  def Annotation  = symbolTable.Annotation

  type Phase = reflect.internal.Phase
  def NoPhase: Phase = reflect.internal.NoPhase

  def SingletonTypeTree = symbolTable.SingletonTypeTree
  def Apply(fun: Tree, args: List[Tree]) = symbolTable.Apply.apply(fun, args)
  def TypeApply = symbolTable.TypeApply
  def AppliedTypeTree = symbolTable.AppliedTypeTree
  def TypeBoundsTree(lo: Tree, hi: Tree) = symbolTable.TypeBoundsTree.apply(lo, hi)

  def EmptyTree = symbolTable.EmptyTree

  def mkNullaryMethodType(res: Type): NullaryMethodType = symbolTable.internal.nullaryMethodType(res)
  def mkMethodType(params: List[Symbol], res: Type): MethodType = symbolTable.internal.methodType(params, res)
  def mkPolyType(params: List[Symbol], res: Type): PolyType = symbolTable.internal.polyType(params, res)
  def mkTypeRef(tpe: Type, sym: Symbol, args: List[Type]): Type = symbolTable.typeRef(tpe, sym, args)
  def mkExistentialType(params: List[Symbol], res: Type): ExistentialType = symbolTable.internal.existentialType(params, res)
  def mkClassInfoType(parents: List[Type], decls: Scope, sym: Symbol): ClassInfoType = symbolTable.internal.classInfoType(parents, decls, sym)
  def mkAppliedType(tycon: Type, args: Type*): Type = symbolTable.appliedType(tycon, args:_*)
  def mkAppliedType(tyconsym: Symbol, args: Type*): Type = symbolTable.appliedType(tyconsym, args:_*)
  def mkThisType(sym: Symbol): Type = symbolTable.internal.thisType(sym)
  def mkTypeBounds(lo: Type, hi: Type): TypeBounds = symbolTable.internal.typeBounds(lo, hi)

  def mkScope: Scope = symbolTable.newScope

  def enteringPhase[T](phase: Phase)(op: => T): T = symbolTable.enteringPhase[T](phase)(op)

  def mkNewFreeTypeSymbol(name: TypeName, flags: FlagSet, origin: String): FreeTypeSymbol = symbolTable.newFreeTypeSymbol(name, flags, origin)

  def mirrorThatLoaded(sym: Symbol): Mirror = symbolTable.mirrorThatLoaded(sym)

  def NoType: Type = symbolTable.NoType

}
