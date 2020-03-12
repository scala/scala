package scala.tools.nsc.tasty.bridge

import scala.tools.nsc
import nsc.symtab
import nsc.tasty.{SafeEq, TastyUniverse}
import nsc.tasty.TastyFlags.{EmptyTastyFlags, TastyFlagSet}
import nsc.tasty.Names.TastyName, TastyName._

import scala.reflect.internal

trait TastyKernel { self: TastyUniverse =>

  type Settings = nsc.Settings

  type SymbolTable <: symtab.SymbolTable { def settings: Settings }

  val symbolTable: SymbolTable

  type FlagSet = symbolTable.FlagSet

  final def isEmpty(flags: FlagSet): Boolean = flags == symbolTable.NoFlags
  final def emptyFlags: FlagSet = symbolTable.NoFlags
  final def emptyTastyFlags: TastyFlagSet = EmptyTastyFlags

  type Reporter = internal.Reporter
  final def reporter: Reporter = symbolTable.reporter

  type Position = symbolTable.Position
  final def noPosition: Position = symbolTable.NoPosition

  private[bridge] final def settings: Settings = symbolTable.settings
  private[bridge] final def phase: Phase = symbolTable.phase

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
  type TypeBounds = symbolTable.TypeBounds
  type RefinedType = symbolTable.RefinedType

  type ConstantType = symbolTable.ConstantType
  final def isConstantType(tpe: Type): Boolean = tpe.isInstanceOf[symbolTable.ConstantType]

  final def errorType: Type = symbolTable.ErrorType
  final def noType: Type = symbolTable.NoType

  final def isError(tpe: Type): Boolean = tpe `eq` symbolTable.ErrorType
  final def isNoType(tpe: Type): Boolean = tpe `eq` symbolTable.NoType

  final def noPrefix: Type = symbolTable.NoPrefix

  object TypeBounds {
    final def unapply(tpe: TypeBounds): Option[(Type, Type)] = symbolTable.TypeBounds.unapply(tpe)
    final def empty: TypeBounds = symbolTable.TypeBounds.empty
    final def upper(hi: Type): TypeBounds = symbolTable.TypeBounds.upper(hi)
    final def lower(lo: Type): TypeBounds = symbolTable.TypeBounds.lower(lo)
    final def bounded(lo: Type, hi: Type): TypeBounds = symbolTable.TypeBounds.apply(lo, hi)
  }

  object GenPolyType {
    final def apply(tparams: List[Symbol], tpe: Type): Type = symbolTable.GenPolyType.apply(tparams,tpe)
    final def unapply(tpe: Type): Option[(List[Symbol], Type)] = symbolTable.GenPolyType.unapply(tpe)
  }

  final def mkSingleType(pre: Type, sym: Symbol): Type = symbolTable.singleType(pre, sym)
  final def mkNullaryMethodType(res: Type): NullaryMethodType = symbolTable.internal.nullaryMethodType(res)
  private[bridge] final def mkMethodType(params: List[Symbol], res: Type): MethodType = symbolTable.internal.methodType(params, res)
  private[bridge] final def mkPolyType(params: List[Symbol], res: Type): PolyType = symbolTable.internal.polyType(params, res)
  private[bridge] final def mkTypeRef(tpe: Type, sym: Symbol, args: List[Type]): Type = symbolTable.typeRef(tpe, sym, args)
  private[bridge] final def mkAppliedType(sym: Symbol, args: List[Type]): Type = symbolTable.appliedType(sym, args)
  private[bridge] final def mkAppliedType(tycon: Type, args: List[Type]): Type = symbolTable.appliedType(tycon, args)
  private[bridge] final def mkExistentialType(params: List[Symbol], res: Type): ExistentialType = symbolTable.internal.existentialType(params, res)
  final def mkClassInfoType(parents: List[Type], decls: Scope, sym: Symbol): ClassInfoType = symbolTable.internal.classInfoType(parents, decls, sym)
  final def mkThisType(sym: Symbol): Type = symbolTable.internal.thisType(sym)
  final def mkConstantType(c: Constant): ConstantType = symbolTable.internal.constantType(c)
  final def mkIntersectionType(tps: Type*): Type = mkIntersectionType(tps.toList)
  final def mkIntersectionType(tps: List[Type]): Type = symbolTable.internal.intersectionType(tps)
  final def mkAnnotatedType(tpe: Type, annot: Annotation): AnnotatedType = symbolTable.AnnotatedType(annot :: Nil, tpe)
  final def mkRefinedType(parents: List[Type], owner: Symbol, scope: Scope): Type = symbolTable.refinedType(parents, owner, scope, noPosition)
  final def mkRefinedType(parents: List[Type], clazz: Symbol): RefinedType = symbolTable.RefinedType.apply(parents, mkScope, clazz)

  final def extensionMethInfo(currentOwner: Symbol, extensionMeth: Symbol, origInfo: Type, clazz: Symbol): Type =
    symbolTable.extensionMethInfo(currentOwner, extensionMeth, origInfo, clazz)

  object defn {
    final val AnyTpe: Type = symbolTable.definitions.AnyTpe
    final val NothingTpe: Type = symbolTable.definitions.NothingTpe
    final val AnyRefTpe: Type = symbolTable.definitions.AnyRefTpe
    final val UnitTpe: Type = symbolTable.definitions.UnitTpe
    final val CompileTimeOnlyAttr: Symbol = symbolTable.definitions.CompileTimeOnlyAttr
    final val ByNameParamClass: ClassSymbol = symbolTable.definitions.ByNameParamClass
    final val ObjectClass: ClassSymbol = symbolTable.definitions.ObjectClass
    final val AnyValClass: ClassSymbol = symbolTable.definitions.AnyValClass
    final val ScalaPackage: ModuleSymbol = symbolTable.definitions.ScalaPackage
    final val ScalaStrictFPAttr: ClassSymbol = symbolTable.definitions.ScalaStrictFPAttr
    final val TailrecClass: ClassSymbol = symbolTable.definitions.TailrecClass
    final val StaticAnnotationClass: ClassSymbol = symbolTable.definitions.StaticAnnotationClass
    final def byNameType(arg: Type): Type = symbolTable.definitions.byNameType(arg)
    final def childAnnotationClass(implicit ctx: Contexts.Context): Option[Symbol] = ctx.loadingMirror.getClassIfDefined("scala.annotation.internal.Child").toOption
    final def arrayType(arg: Type): Type = symbolTable.definitions.arrayType(arg)
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
    final val WILDCARD: TermName = symbolTable.nme.WILDCARD
    final def freshWhileName: TermName = symbolTable.freshTermName(symbolTable.nme.WHILE_PREFIX)(symbolTable.currentFreshNameCreator)
  }

  object termNames {
    final val EMPTY: TermName = symbolTable.termNames.EMPTY
  }

  private[bridge] type LazyType = symbolTable.LazyType
  private[bridge] type FlagAgnosticCompleter = symbolTable.FlagAgnosticCompleter

  final def noSymbol: Symbol = symbolTable.NoSymbol

  type Scope = symbolTable.Scope
  final def emptyScope: Scope = symbolTable.EmptyScope
  final def mkScope(syms: Symbol*): Scope = symbolTable.newScopeWith(syms:_*)
  final def mkScope: Scope = symbolTable.newScope

  type Symbol = symbolTable.Symbol
  type MethodSymbol = symbolTable.MethodSymbol
  type TermSymbol = symbolTable.TermSymbol
  type ModuleSymbol = symbolTable.ModuleSymbol
  type ClassSymbol = symbolTable.ClassSymbol
  type FreeTypeSymbol = symbolTable.FreeTypeSymbol
  type RefinementClassSymbol = symbolTable.RefinementClassSymbol

  type Constant = symbolTable.Constant
  final def Constant(value: Any): Constant = symbolTable.Constant(value)

  type Mirror = symbolTable.Mirror

  type Name   = symbolTable.Name
  type TermName = symbolTable.TermName
  type TypeName = symbolTable.TypeName

  final def showRaw(flags: Long): String = symbolTable.showRaw(flags)
  final def showRaw(any: Product): String = symbolTable.showRaw(any)

  final def mkTermName(str: String): TermName = symbolTable.TermName(str)

  type Tree = symbolTable.Tree

  type RefTree = symbolTable.RefTree
  final def RefTree(qual: Tree, name: Name): RefTree = symbolTable.RefTree(qual, name)

  type TypeTree = symbolTable.TypeTree
  final def TypeTree(tp: Type): TypeTree = symbolTable.TypeTree(tp)

  type This = symbolTable.This

  type SeqLiteral = symbolTable.ArrayValue
  final def SeqLiteral(trees: List[Tree], tpt: Tree) = symbolTable.ArrayValue(tpt, trees)

  type Typed = symbolTable.Typed
  final def Typed(expr: Tree, tpt: Tree): Typed = symbolTable.Typed(expr, tpt)

  type Literal = symbolTable.Literal
  final def Literal(c: Constant): Literal = symbolTable.Literal(c)

  type Ident = symbolTable.Ident
  final def Ident(name: Name): Ident = symbolTable.Ident(name)

  type New = symbolTable.New
  final def New(tpt: Tree): New = symbolTable.New(tpt)

  type If = symbolTable.If
  final def If(cond: Tree, thenp: Tree, elsep: Tree): If = symbolTable.If(cond, thenp, elsep)

  type Select = symbolTable.Select
  final def Select(qual: Tree, name: Name): Select = symbolTable.Select(qual, name)

  type Annotation = symbolTable.Annotation
  final def mkAnnotation(tree: Tree): Annotation = {tree match {
    case symbolTable.Apply(symbolTable.Select(symbolTable.New(tpt), nme.CONSTRUCTOR), args) =>
      symbolTable.AnnotationInfo(tpt.tpe, args, Nil)
    case _ =>
      throw new Exception("unexpected annotation kind from TASTy")
  }}

  type Phase = reflect.internal.Phase
  final def isNoPhase(phase: Phase): Boolean = phase `eq` reflect.internal.NoPhase

  type SingletonTypeTree = symbolTable.SingletonTypeTree
  final def SingletonTypeTree(ref: Tree): SingletonTypeTree = symbolTable.SingletonTypeTree(ref)

  type Apply = symbolTable.Apply
  final def Apply(fun: Tree, args: List[Tree]): Apply = symbolTable.Apply(fun, args)

  type TypeApply = symbolTable.TypeApply
  final def TypeApply(fun: Tree, args: List[Tree]): TypeApply = symbolTable.TypeApply(fun, args)

  type AppliedTypeTree = symbolTable.AppliedTypeTree
  final def AppliedTypeTree(tpt: Tree, args: List[Tree]): AppliedTypeTree = symbolTable.AppliedTypeTree(tpt, args)

  type TypeBoundsTree = symbolTable.TypeBoundsTree
  final def TypeBoundsTree(lo: Tree, hi: Tree): TypeBoundsTree = symbolTable.TypeBoundsTree(lo, hi)

  type CompoundTypeTree = symbolTable.CompoundTypeTree
  final def CompoundTypeTree(tps: List[Tree]): CompoundTypeTree = symbolTable.CompoundTypeTree(symbolTable.Template(tps, symbolTable.noSelfType, Nil))

  type NamedArg = symbolTable.NamedArg
  final def NamedArg(name: Name, value: Tree): NamedArg = symbolTable.NamedArg(Ident(name), value)

  type Block = symbolTable.Block
  final def Block(stats: List[Tree], value: Tree): Block = symbolTable.Block((stats :+ value):_*)

  type CaseDef = symbolTable.CaseDef
  final def CaseDef(pat: Tree, guard: Tree, body: Tree): CaseDef = symbolTable.CaseDef(pat, guard, body)

  type Bind = symbolTable.Bind
  final def Bind(sym: Symbol, body: Tree): Bind = symbolTable.Bind(sym.name, body)

  type Match = symbolTable.Match
  final def Match(selector: Tree, cases: List[CaseDef]): Match = symbolTable.Match(selector: Tree, cases: List[CaseDef])

  type Alternative = symbolTable.Alternative
  final def Alternative(alts: List[Tree]): Alternative = symbolTable.Alternative(alts)

  type UnApply = symbolTable.UnApply
  final def UnApply(fun: Tree, implicitArgs: List[Tree], args: List[Tree], patType: Type): UnApply = {
    val tree = implicitArgs match {
      case Nil => fun
      case _   => Apply(fun, implicitArgs).setType(fun.tpe.resultType)
    }
    symbolTable.UnApply(tree, args).setType(patType)
  }

  type Annotated = symbolTable.Annotated
  final def Annotated(tpt: Tree, annot: Tree): Annotated = symbolTable.Annotated(annot, tpt)

  type Throw = symbolTable.Throw
  final def Throw(err: Tree): Throw = symbolTable.Throw(err)

  type Assign = symbolTable.Assign
  final def Assign(ref: Tree, value: Tree): Assign = symbolTable.Assign(ref, value)

  type WhileDo = symbolTable.LabelDef
  final def WhileDo(cond: Tree, body: Tree): WhileDo = {
    val label    = nme.freshWhileName
    val ref      = Ident(label).setType(defn.UnitTpe)
    val rec      = Apply(ref, Nil).setType(defn.UnitTpe)
    val loop     = Block(body :: Nil, rec).setType(defn.UnitTpe)
    val unitExpr = symbolTable.gen.mkTuple(Nil).setType(defn.UnitTpe)
    val whileDo  = If(cond, loop, unitExpr).setType(defn.UnitTpe)
    symbolTable.LabelDef(label, Nil, whileDo).setType(defn.UnitTpe)
  }

  type Try = symbolTable.Try
  final def Try(body: Tree, cases: List[CaseDef], finalizer: Tree): Try = symbolTable.Try(body, cases, finalizer)

  final def mkFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree = symbolTable.gen.mkFunctionTypeTree(argtpes, restpe)

  final def emptyTree: Tree = symbolTable.EmptyTree

  final def withPhaseNoLater[T](phase: Phase)(op: => T): T = symbolTable.enteringPhaseNotLaterThan[T](phase)(op)

  final def mirrorThatLoaded(sym: Symbol): Mirror = symbolTable.mirrorThatLoaded(sym)

  final def lub(tpe1: Type, tpe2: Type): Type = symbolTable.lub(tpe1 :: tpe2 :: Nil)
  final def lub(tpes: List[Type]): Type = symbolTable.lub(tpes)
}
