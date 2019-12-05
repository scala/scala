package scala.tools.nsc.tasty.bridge

import scala.reflect.internal
import internal.SymbolTable, internal.settings.MutableSettings

import scala.tools.nsc.tasty.TastyFlags.{ EmptyTastyFlags, TastyFlagSet }

trait TastyKernel {

  val symbolTable: SymbolTable

  type FlagSet = symbolTable.FlagSet

  def isEmpty(flags: FlagSet): Boolean = flags == symbolTable.NoFlags
  def emptyFlags: FlagSet = symbolTable.NoFlags
  def emptyTastyFlags: TastyFlagSet = EmptyTastyFlags

  type Reporter = internal.Reporter
  def reporter: Reporter = symbolTable.reporter

  type Postion = symbolTable.Position
  def noPosition: Postion = symbolTable.NoPosition

  type Settings = MutableSettings
  private[bridge] def settings: Settings = symbolTable.settings
  private[bridge] def phase: Phase = symbolTable.phase

  type Type = symbolTable.Type
  type ClassInfoType = symbolTable.ClassInfoType
  type ExistentialType = symbolTable.ExistentialType
  type NullaryMethodType = symbolTable.NullaryMethodType
  type MethodType = symbolTable.MethodType
  type PolyType = symbolTable.PolyType
  type ThisType = symbolTable.ThisType
  type TypeRef = symbolTable.TypeRef

  type ConstantType = symbolTable.ConstantType
  def isConstantType(tpe: Type): Boolean = tpe.isInstanceOf[symbolTable.ConstantType]

  def errorType: Type = symbolTable.ErrorType
  def noType: Type = symbolTable.NoType

  def isError(tpe: Type): Boolean = tpe `eq` symbolTable.ErrorType
  def isNoType(tpe: Type): Boolean = tpe `eq` symbolTable.NoType

  def noPrefix: Type = symbolTable.NoPrefix

  type TypeBounds = symbolTable.TypeBounds
  def emptyTypeBounds: TypeBounds = symbolTable.TypeBounds.empty

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
  def mkConstantType(c: Constant): ConstantType = symbolTable.internal.constantType(c)

  object defn {
    val NothingTpe: Type = symbolTable.definitions.NothingTpe
    val AnyRefTpe: Type = symbolTable.definitions.AnyRefTpe
    def byNameType(arg: Type): Type = symbolTable.definitions.byNameType(arg)
    val ByNameParamClass: ClassSymbol = symbolTable.definitions.ByNameParamClass
    val ObjectClass: ClassSymbol = symbolTable.definitions.ObjectClass
    val AnyValClass: ClassSymbol = symbolTable.definitions.AnyValClass
  }

  object nme {
    val EMPTY: TermName = symbolTable.nme.EMPTY
    val CONSTRUCTOR: TermName = symbolTable.nme.CONSTRUCTOR
    val ROOT: TermName = symbolTable.nme.ROOT
    val ROOTPKG: TermName = symbolTable.nme.ROOTPKG
    val EMPTY_PACKAGE_NAME: TermName = symbolTable.nme.EMPTY_PACKAGE_NAME
  }

  object termNames {
    val EMPTY: TermName = symbolTable.termNames.EMPTY
  }

  private[bridge] type LazyType = symbolTable.LazyType
  private[bridge] type FlagAgnosticCompleter = symbolTable.FlagAgnosticCompleter

  def noSymbol: Symbol = symbolTable.NoSymbol

  type Scope = symbolTable.Scope
  def emptyScope: Scope = symbolTable.EmptyScope

  type Symbol = symbolTable.Symbol
  type TermSymbol = symbolTable.TermSymbol
  type ModuleSymbol = symbolTable.ModuleSymbol
  type ClassSymbol = symbolTable.ClassSymbol
  type FreeTypeSymbol = symbolTable.FreeTypeSymbol

  type Constant = symbolTable.Constant
  def Constant(value: Any): Constant = symbolTable.Constant(value)

  type Mirror = symbolTable.Mirror

  type Name   = symbolTable.Name
  type TermName = symbolTable.TermName
  type TypeName = symbolTable.TypeName

  def showRaw(flags: Long): String = symbolTable.showRaw(flags)
  def showRaw(any: Product): String = symbolTable.showRaw(any)

  def mkTermName(str: String): TermName = {
    import symbolTable._
    str
  }

  type Tree = symbolTable.Tree

  type RefTree = symbolTable.RefTree
  def RefTree(qual: Tree, name: Name): RefTree = symbolTable.RefTree(qual, name)

  type TypeTree = symbolTable.TypeTree
  def TypeTree(tp: Type): TypeTree = symbolTable.TypeTree(tp)

  type This = symbolTable.This

  type Literal = symbolTable.Literal
  def Literal(c: Constant): Literal = symbolTable.Literal(c)

  type Ident = symbolTable.Ident
  def Ident(name: Name): Ident = symbolTable.Ident(name)

  type New = symbolTable.New
  def New(tpt: Tree): New = symbolTable.New(tpt)

  type Select = symbolTable.Select
  def Select(qual: Tree, name: Name): Select = symbolTable.Select(qual, name)

  type Annotation = symbolTable.Annotation
  def Annotation(tree: Tree): Annotation = symbolTable.Annotation(tree)

  type Phase = reflect.internal.Phase
  def isNoPhase(phase: Phase): Boolean = phase `eq` reflect.internal.NoPhase

  type SingletonTypeTree = symbolTable.SingletonTypeTree
  def SingletonTypeTree(ref: Tree): SingletonTypeTree = symbolTable.SingletonTypeTree(ref)

  type Apply = symbolTable.Apply
  def Apply(fun: Tree, args: List[Tree]): Apply = symbolTable.Apply(fun, args)

  type TypeApply = symbolTable.TypeApply
  def TypeApply(fun: Tree, args: List[Tree]): TypeApply = symbolTable.TypeApply(fun, args)

  type AppliedTypeTree = symbolTable.AppliedTypeTree
  def AppliedTypeTree(tpt: Tree, args: List[Tree]): AppliedTypeTree = symbolTable.AppliedTypeTree(tpt, args)

  type TypeBoundsTree = symbolTable.TypeBoundsTree
  def TypeBoundsTree(lo: Tree, hi: Tree): TypeBoundsTree = symbolTable.TypeBoundsTree(lo, hi)

  def mkFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree = symbolTable.gen.mkFunctionTypeTree(argtpes, restpe)

  def emptyTree: Tree = symbolTable.EmptyTree

  def mkScope: Scope = symbolTable.newScope

  def enteringPhase[T](phase: Phase)(op: => T): T = symbolTable.enteringPhase[T](phase)(op)

  def mkNewFreeTypeSymbol(name: TypeName, flags: FlagSet, origin: String): FreeTypeSymbol = symbolTable.newFreeTypeSymbol(name, flags, origin)

  def mirrorThatLoaded(sym: Symbol): Mirror = symbolTable.mirrorThatLoaded(sym)
}
