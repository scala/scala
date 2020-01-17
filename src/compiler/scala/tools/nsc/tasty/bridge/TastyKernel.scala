package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.SafeEq
import scala.reflect.internal
import internal.SymbolTable, internal.settings.MutableSettings

import scala.tools.nsc.tasty.TastyFlags.{ EmptyTastyFlags, TastyFlagSet }
import scala.tools.nsc.tasty.Names.TastyName
import TastyName._
import scala.tools.nsc.tasty.TastyUniverse

trait TastyKernel { self: TastyUniverse =>

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
  type SingleType = symbolTable.SingleType
  type AnnotatedType = symbolTable.AnnotatedType

  type ConstantType = symbolTable.ConstantType
  def isConstantType(tpe: Type): Boolean = tpe.isInstanceOf[symbolTable.ConstantType]

  def errorType: Type = symbolTable.ErrorType
  def noType: Type = symbolTable.NoType

  def isError(tpe: Type): Boolean = tpe `eq` symbolTable.ErrorType
  def isNoType(tpe: Type): Boolean = tpe `eq` symbolTable.NoType

  def noPrefix: Type = symbolTable.NoPrefix

  type TypeBounds = symbolTable.TypeBounds
  def emptyTypeBounds: TypeBounds = symbolTable.TypeBounds.empty

  def cloneSymbolsAtOwner(syms: List[Symbol], owner: Symbol): List[Symbol] =
    symbolTable.cloneSymbolsAtOwner(syms,owner)

  def defineOriginalOwner(sym: Symbol, owner: Symbol): Unit = symbolTable.defineOriginalOwner(sym, owner)


  object GenPolyType {
    def apply(tparams: List[Symbol], tpe: Type): Type = symbolTable.GenPolyType.apply(tparams,tpe)
    def unapply(tpe: Type): Option[(List[Symbol], Type)] = symbolTable.GenPolyType.unapply(tpe)
  }

  def dropNullaryMethod(tp: Type): Type = symbolTable.definitions.dropNullaryMethod(tp)

  def mkSingleType(pre: Type, sym: Symbol): Type = symbolTable.singleType(pre, sym)
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
  def mkIntersectionType(tps: Type*): Type = mkIntersectionType(tps.toList)
  def mkIntersectionType(tps: List[Type]): Type = symbolTable.internal.intersectionType(tps)
  def mkAnnotatedType(tpe: Type, annot: Annotation): AnnotatedType = symbolTable.AnnotatedType(annot :: Nil, tpe)

  def extensionMethInfo(currentOwner: Symbol, extensionMeth: Symbol, origInfo: Type, clazz: Symbol): Type =
    symbolTable.extensionMethInfo(currentOwner, extensionMeth, origInfo, clazz)

  object defn {
    def byNameType(arg: Type): Type = symbolTable.definitions.byNameType(arg)
    final val NothingTpe: Type = symbolTable.definitions.NothingTpe
    final val AnyRefTpe: Type = symbolTable.definitions.AnyRefTpe
    final val UnitTpe: Type = symbolTable.definitions.UnitTpe
    final val ByNameParamClass: ClassSymbol = symbolTable.definitions.ByNameParamClass
    final val ObjectClass: ClassSymbol = symbolTable.definitions.ObjectClass
    final val AnyValClass: ClassSymbol = symbolTable.definitions.AnyValClass
    final val ScalaPackage: ModuleSymbol = symbolTable.definitions.ScalaPackage
    final val ScalaStrictFPAttr: ClassSymbol = symbolTable.definitions.ScalaStrictFPAttr
    final val TailrecClass: ClassSymbol = symbolTable.definitions.TailrecClass
    final val StaticAnnotationClass: ClassSymbol = symbolTable.definitions.StaticAnnotationClass
    def childAnnotationClass(implicit ctx: Contexts.Context): Option[Symbol] =
      ctx.loadingMirror.getClassIfDefined("scala.annotation.internal.Child").toOption
    def arrayType(arg: Type): Type = symbolTable.definitions.arrayType(arg)
    // final val BooleanTpe: Type = symbolTable.definitions.BooleanTpe
    // def optionType(value: Type) = symbolTable.definitions.optionType(value)
    // def tupleType(values: List[Type]) = symbolTable.definitions.tupleType(values)
  }

  object nme {
    final val Or = SimpleName("|")
    final val And = SimpleName("&")
    final val EMPTY: TermName = symbolTable.nme.EMPTY
    final val SELF: TermName = symbolTable.nme.SELF
    final val CONSTRUCTOR: TermName = symbolTable.nme.CONSTRUCTOR
    final val ROOT: TermName = symbolTable.nme.ROOT
    final val ROOTPKG: TermName = symbolTable.nme.ROOTPKG
    final val EMPTY_PACKAGE_NAME: TermName = symbolTable.nme.EMPTY_PACKAGE_NAME
    def freshWhileName: TermName = symbolTable.freshTermName(symbolTable.nme.WHILE_PREFIX)(symbolTable.currentFreshNameCreator)
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

  type SeqLiteral = symbolTable.ArrayValue
  def SeqLiteral(trees: List[Tree], tpt: Tree) = symbolTable.ArrayValue(tpt, trees)

  type Typed = symbolTable.Typed
  def Typed(expr: Tree, tpt: Tree): Typed = symbolTable.Typed(expr, tpt)

  type Literal = symbolTable.Literal
  def Literal(c: Constant): Literal = symbolTable.Literal(c)

  type Ident = symbolTable.Ident
  def Ident(name: Name): Ident = symbolTable.Ident(name)

  type New = symbolTable.New
  def New(tpt: Tree): New = symbolTable.New(tpt)

  type If = symbolTable.If
  def If(cond: Tree, thenp: Tree, elsep: Tree): If = symbolTable.If(cond, thenp, elsep)

  type Select = symbolTable.Select
  def Select(qual: Tree, name: Name): Select = symbolTable.Select(qual, name)

  type Annotation = symbolTable.Annotation
  def mkAnnotation(tree: Tree): Annotation = {tree match {
    case symbolTable.Apply(symbolTable.Select(symbolTable.New(tpt), nme.CONSTRUCTOR), args) =>
      symbolTable.AnnotationInfo(tpt.tpe, args, Nil)
    case _ =>
      throw new Exception("unexpected annotation kind from TASTy")
  }}

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

  type CompoundTypeTree = symbolTable.CompoundTypeTree
  def CompoundTypeTree(tps: List[Tree]): CompoundTypeTree = symbolTable.CompoundTypeTree(symbolTable.Template(tps, symbolTable.noSelfType, Nil))

  type NamedArg = symbolTable.NamedArg
  def NamedArg(name: Name, value: Tree): NamedArg = symbolTable.NamedArg(Ident(name), value)

  type Block = symbolTable.Block
  def Block(stats: List[Tree], value: Tree): Block = symbolTable.Block((stats :+ value):_*)

  type CaseDef = symbolTable.CaseDef
  def CaseDef(pat: Tree, guard: Tree, body: Tree): CaseDef = symbolTable.CaseDef(pat, guard, body)

  type Bind = symbolTable.Bind
  def Bind(sym: Symbol, body: Tree): Bind = symbolTable.Bind(sym.name, body)

  type Match = symbolTable.Match
  def Match(selector: Tree, cases: List[CaseDef]): Match = symbolTable.Match(selector: Tree, cases: List[CaseDef])

  type Alternative = symbolTable.Alternative
  def Alternative(alts: List[Tree]): Alternative = symbolTable.Alternative(alts)

  type UnApply = symbolTable.UnApply
  def UnApply(fun: Tree, implicitArgs: List[Tree], args: List[Tree], patType: Type): UnApply = {
    val tree = implicitArgs match {
      case Nil => fun
      case _   => Apply(fun, implicitArgs).setType(fun.tpe.resultType)
    }
    symbolTable.UnApply(tree, args).setType(patType)
  }

  type Annotated = symbolTable.Annotated
  def Annotated(tpt: Tree, annot: Tree): Annotated = symbolTable.Annotated(annot, tpt)

  type Throw = symbolTable.Throw
  def Throw(err: Tree): Throw = symbolTable.Throw(err)

  type Assign = symbolTable.Assign
  def Assign(ref: Tree, value: Tree): Assign = symbolTable.Assign(ref, value)

  type WhileDo = symbolTable.LabelDef
  def WhileDo(cond: Tree, body: Tree): WhileDo = {
    val label    = nme.freshWhileName
    val ref      = Ident(label).setType(defn.UnitTpe)
    val rec      = Apply(ref, Nil).setType(defn.UnitTpe)
    val loop     = Block(body :: Nil, rec).setType(defn.UnitTpe)
    val unitExpr = symbolTable.gen.mkTuple(Nil).setType(defn.UnitTpe)
    val whileDo  = If(cond, loop, unitExpr).setType(defn.UnitTpe)
    symbolTable.LabelDef(label, Nil, whileDo).setType(defn.UnitTpe)
  }

  type Try = symbolTable.Try
  def Try(body: Tree, cases: List[CaseDef], finalizer: Tree): Try = symbolTable.Try(body, cases, finalizer)

  def mkFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree = symbolTable.gen.mkFunctionTypeTree(argtpes, restpe)

  def emptyTree: Tree = symbolTable.EmptyTree

  def mkScope: Scope = symbolTable.newScope

  def enteringPhase[T](phase: Phase)(op: => T): T = symbolTable.enteringPhase[T](phase)(op)

  def mkNewFreeTypeSymbol(name: TypeName, flags: FlagSet, origin: String): FreeTypeSymbol = symbolTable.newFreeTypeSymbol(name, flags, origin)

  def mirrorThatLoaded(sym: Symbol): Mirror = symbolTable.mirrorThatLoaded(sym)

  def lub(tpe1: Type, tpe2: Type): Type = symbolTable.lub(tpe1 :: tpe2 :: Nil)
  def lub(tpes: List[Type]): Type = symbolTable.lub(tpes)
}
