package scala.tools.nsc.tasty.bridge

import scala.tools.nsc
import nsc.symtab
import nsc.tasty.TastyUniverse
import nsc.tasty.TastyFlags.{EmptyTastyFlags, TastyFlagSet}
import nsc.tasty.TastyName

import scala.util.chaining._

abstract class TastyKernel { self: TastyUniverse =>
  import self.{symbolTable => u}

  type SymbolTable <: symtab.SymbolTable { def settings: nsc.Settings }

  val symbolTable: SymbolTable

  type FlagSet = u.FlagSet

  @inline final def isEmpty(flags: FlagSet): Boolean = flags == u.NoFlags
  @inline final def emptyFlags: FlagSet = u.NoFlags
  @inline final def emptyTastyFlags: TastyFlagSet = EmptyTastyFlags

  type Position = u.Position
  @inline final def noPosition: Position = u.NoPosition

  @inline final def picklerPhase: Phase = u.picklerPhase
  @inline final def extmethodsPhase: Phase = u.findPhaseWithName("extmethods")

  type Type = u.Type
  type ClassInfoType = u.ClassInfoType
  type ExistentialType = u.ExistentialType
  type NullaryMethodType = u.NullaryMethodType
  type MethodType = u.MethodType
  type PolyType = u.PolyType
  type ThisType = u.ThisType
  type TypeRef = u.TypeRef
  type SingleType = u.SingleType
  type AnnotatedType = u.AnnotatedType
  type TypeBounds = u.TypeBounds
  type RefinedType = u.RefinedType

  type ConstantType = u.ConstantType
  @inline final def isConstantType(tpe: Type): Boolean = tpe.isInstanceOf[u.ConstantType]

  @inline final def errorType: Type = u.ErrorType
  @inline final def noType: Type = u.NoType

  @inline final def isError(tpe: Type): Boolean = tpe `eq` u.ErrorType
  @inline final def isNoType(tpe: Type): Boolean = tpe `eq` u.NoType

  @inline final def noPrefix: Type = u.NoPrefix

  object TypeBounds {
    @inline final def unapply(tpe: TypeBounds): Option[(Type, Type)] = u.TypeBounds.unapply(tpe)
    @inline final def empty: TypeBounds = u.TypeBounds.empty
    @inline final def upper(hi: Type): TypeBounds = u.TypeBounds.upper(hi)
    @inline final def lower(lo: Type): TypeBounds = u.TypeBounds.lower(lo)
    @inline final def bounded(lo: Type, hi: Type): TypeBounds = u.TypeBounds.apply(lo, hi)
  }

  object GenPolyType {
    @inline final def apply(tparams: List[Symbol], tpe: Type): Type = u.GenPolyType.apply(tparams,tpe)
    @inline final def unapply(tpe: Type): Option[(List[Symbol], Type)] = u.GenPolyType.unapply(tpe)
  }

  @inline final def mkSingleType(pre: Type, sym: Symbol): Type = u.singleType(pre, sym)
  @inline final def mkNullaryMethodType(res: Type): NullaryMethodType = u.internal.nullaryMethodType(res)
  @inline final def mkPolyType(params: List[Symbol], res: Type): PolyType = u.internal.polyType(params, res)
  @inline final def mkClassInfoType(parents: List[Type], decls: Scope, sym: Symbol): ClassInfoType = u.internal.classInfoType(parents, decls, sym)
  @inline final def mkThisType(sym: Symbol): Type = u.internal.thisType(sym)
  @inline final def mkConstantType(c: Constant): ConstantType = u.internal.constantType(c)
  @inline final def mkIntersectionType(tps: Type*): Type = mkIntersectionType(tps.toList)
  @inline final def mkIntersectionType(tps: List[Type]): Type = u.internal.intersectionType(tps)
  @inline final def mkAnnotatedType(tpe: Type, annot: Annotation): AnnotatedType = u.AnnotatedType(annot :: Nil, tpe)
  @inline final def mkRefinedTypeWith(parents: List[Type], clazz: Symbol, decls: Scope): RefinedType = u.RefinedType.apply(parents, decls, clazz).tap(clazz.info = _)
  @inline final def mkRefinedType(parents: List[Type], clazz: Symbol): RefinedType = mkRefinedTypeWith(parents, clazz, mkScope)
  @inline final def mkSuperType(thisTpe: Type, superTpe: Type): Type = u.SuperType(thisTpe, superTpe)

  @inline final def extensionMethInfo(currentOwner: Symbol, extensionMeth: Symbol, origInfo: Type, clazz: Symbol): Type =
    u.extensionMethInfo(currentOwner, extensionMeth, origInfo, clazz)

  object defn {
    final val AnyTpe: Type = u.definitions.AnyTpe
    final val NothingTpe: Type = u.definitions.NothingTpe
    final val AnyRefTpe: Type = u.definitions.AnyRefTpe
    final val UnitTpe: Type = u.definitions.UnitTpe
    final val JavaEnumClass: ClassSymbol = u.definitions.JavaEnumClass
    final val CompileTimeOnlyAttr: Symbol = u.definitions.CompileTimeOnlyAttr
    final val ByNameParamClass: ClassSymbol = u.definitions.ByNameParamClass
    final val ObjectClass: ClassSymbol = u.definitions.ObjectClass
    final val AnyValClass: ClassSymbol = u.definitions.AnyValClass
    final val ScalaPackage: ModuleSymbol = u.definitions.ScalaPackage
    final val ScalaStrictFPAttr: ClassSymbol = u.definitions.ScalaStrictFPAttr
    final val TailrecClass: ClassSymbol = u.definitions.TailrecClass
    final val StaticAnnotationClass: ClassSymbol = u.definitions.StaticAnnotationClass
    final val SeqClass: ClassSymbol = u.definitions.SeqClass
    @inline final def byNameType(arg: Type): Type = u.definitions.byNameType(arg)
    @inline final def scalaRepeatedType(arg: Type): Type = u.definitions.scalaRepeatedType(arg)
    @inline final def repeatedAnnotationClass(implicit ctx: Context): Option[Symbol] = ctx.loadingMirror.getClassIfDefined("scala.annotation.internal.Repeated").toOption
    @inline final def childAnnotationClass(implicit ctx: Context): Option[Symbol] = ctx.loadingMirror.getClassIfDefined("scala.annotation.internal.Child").toOption
    @inline final def arrayType(dims: Int, arg: Type): Type = (0 until dims).foldLeft(arg)((acc, _) => u.definitions.arrayType(acc))
  }

  object nme {
    final val Or: TastyName.SimpleName = TastyName.SimpleName("|")
    final val And: TastyName.SimpleName = TastyName.SimpleName("&")
    final val EMPTY: TermName = u.nme.EMPTY
    final val SELF: TermName = u.nme.SELF
    final val CONSTRUCTOR: TermName = u.nme.CONSTRUCTOR
    final val ROOT: TermName = u.nme.ROOT
    final val ROOTPKG: TermName = u.nme.ROOTPKG
    final val EMPTY_PACKAGE_NAME: TermName = u.nme.EMPTY_PACKAGE_NAME
    final val WILDCARD: TermName = u.nme.WILDCARD
    @inline final def freshWhileName: TermName = u.freshTermName(u.nme.WHILE_PREFIX)(u.currentFreshNameCreator)
  }

  object tpnme {
    final val REPEATED_PARAM_CLASS_NAME: TypeName = u.tpnme.REPEATED_PARAM_CLASS_NAME
  }

  object untpd {
    final val EmptyTypeIdent: Ident = new Ident(nme.EMPTY) {
      override def isEmpty: Boolean = true
    }
  }

  object termNames {
    final val EMPTY: TermName = u.termNames.EMPTY
  }

  @inline final def noSymbol: Symbol = u.NoSymbol
  @inline final def isSymbol(sym: Symbol): Boolean = sym ne u.NoSymbol

  type Scope = u.Scope
  @inline final def emptyScope: Scope = u.EmptyScope
  @inline final def mkScope(syms: Symbol*): Scope = u.newScopeWith(syms:_*)
  @inline final def mkScope: Scope = u.newScope

  type Symbol = u.Symbol
  type MethodSymbol = u.MethodSymbol
  type TermSymbol = u.TermSymbol
  type ModuleSymbol = u.ModuleSymbol
  type ClassSymbol = u.ClassSymbol
  type FreeTypeSymbol = u.FreeTypeSymbol
  type RefinementClassSymbol = u.RefinementClassSymbol

  type Constant = u.Constant
  @inline final def Constant(value: Any): Constant = u.Constant(value)

  type Mirror = u.Mirror

  type Name   = u.Name
  type TermName = u.TermName
  type TypeName = u.TypeName

  @inline final def showRaw(flags: Long): String = u.showRaw(flags)
  @inline final def showRaw(any: Product): String = u.showRaw(any)

  @inline final def mkTermName(str: String): TermName = u.TermName(str)

  type Tree = u.Tree

  type RefTree = u.RefTree
  @inline final def RefTree(qual: Tree, name: Name): RefTree = u.RefTree(qual, name)

  type TypeTree = u.TypeTree
  @inline final def TypeTree(tp: Type): TypeTree = u.TypeTree(tp)

  type This = u.This

  type SeqLiteral = u.ArrayValue
  @inline final def SeqLiteral(trees: List[Tree], tpt: Tree) = u.ArrayValue(tpt, trees)

  type Typed = u.Typed
  @inline final def Typed(expr: Tree, tpt: Tree): Typed = u.Typed(expr, tpt)

  type Literal = u.Literal
  @inline final def Literal(c: Constant): Literal = u.Literal(c)

  type Ident = u.Ident
  @inline final def Ident(name: Name): Ident = u.Ident(name)

  type New = u.New
  @inline final def New(tpt: Tree): New = u.New(tpt)

  type If = u.If
  @inline final def If(cond: Tree, thenp: Tree, elsep: Tree): If = u.If(cond, thenp, elsep)

  type Select = u.Select
  @inline final def Select(qual: Tree, name: Name): Select = u.Select(qual, name)

  type Annotation = u.Annotation
  @inline final def mkAnnotation(tree: Tree): Annotation = {tree match {
    case u.Apply(u.Select(u.New(tpt), nme.CONSTRUCTOR), args) =>
      u.AnnotationInfo(tpt.tpe, args, Nil)
    case _ =>
      throw new Exception("unexpected annotation kind from TASTy")
  }}

  type Phase = reflect.internal.Phase
  @inline final def isNoPhase(phase: Phase): Boolean = phase `eq` reflect.internal.NoPhase

  type SingletonTypeTree = u.SingletonTypeTree
  @inline final def SingletonTypeTree(ref: Tree): SingletonTypeTree = u.SingletonTypeTree(ref)

  type Apply = u.Apply
  @inline final def Apply(fun: Tree, args: List[Tree]): Apply = u.Apply(fun, args)

  type TypeApply = u.TypeApply
  @inline final def TypeApply(fun: Tree, args: List[Tree]): TypeApply = u.TypeApply(fun, args)

  type AppliedTypeTree = u.AppliedTypeTree
  @inline final def AppliedTypeTree(tpt: Tree, args: List[Tree]): AppliedTypeTree = u.AppliedTypeTree(tpt, args)

  type Super = u.Super
  @inline final def Super(qual: Tree, mix: TypeName): Super = u.Super(qual, mix)

  type TypeBoundsTree = u.TypeBoundsTree
  @inline final def TypeBoundsTree(lo: Tree, hi: Tree): TypeBoundsTree = u.TypeBoundsTree(lo, hi)

  type CompoundTypeTree = u.CompoundTypeTree
  @inline final def CompoundTypeTree(tps: List[Tree]): CompoundTypeTree = u.CompoundTypeTree(u.Template(tps, u.noSelfType, Nil))

  type NamedArg = u.NamedArg
  @inline final def NamedArg(name: Name, value: Tree): NamedArg = u.NamedArg(Ident(name), value)

  type Block = u.Block
  @inline final def Block(stats: List[Tree], value: Tree): Block = u.Block((stats :+ value):_*)

  type CaseDef = u.CaseDef
  @inline final def CaseDef(pat: Tree, guard: Tree, body: Tree): CaseDef = u.CaseDef(pat, guard, body)

  type Bind = u.Bind
  @inline final def Bind(sym: Symbol, body: Tree): Bind = u.Bind(sym.name, body)

  type Match = u.Match
  @inline final def Match(selector: Tree, cases: List[CaseDef]): Match = u.Match(selector: Tree, cases: List[CaseDef])

  type Alternative = u.Alternative
  @inline final def Alternative(alts: List[Tree]): Alternative = u.Alternative(alts)

  type UnApply = u.UnApply
  @inline final def UnApply(fun: Tree, implicitArgs: List[Tree], args: List[Tree], patType: Type): UnApply = {
    val tree = implicitArgs match {
      case Nil => fun
      case _   => Apply(fun, implicitArgs).setType(fun.tpe.resultType)
    }
    u.UnApply(tree, args).setType(patType)
  }

  type Annotated = u.Annotated
  @inline final def Annotated(tpt: Tree, annot: Tree): Annotated = u.Annotated(annot, tpt)

  type Throw = u.Throw
  @inline final def Throw(err: Tree): Throw = u.Throw(err)

  type Assign = u.Assign
  @inline final def Assign(ref: Tree, value: Tree): Assign = u.Assign(ref, value)

  type WhileDo = u.LabelDef
  @inline final def WhileDo(cond: Tree, body: Tree): WhileDo = {
    val label    = nme.freshWhileName
    val ref      = Ident(label).setType(defn.UnitTpe)
    val rec      = Apply(ref, Nil).setType(defn.UnitTpe)
    val loop     = Block(body :: Nil, rec).setType(defn.UnitTpe)
    val unitExpr = u.gen.mkTuple(Nil).setType(defn.UnitTpe)
    val whileDo  = If(cond, loop, unitExpr).setType(defn.UnitTpe)
    u.LabelDef(label, Nil, whileDo).setType(defn.UnitTpe)
  }

  type Try = u.Try
  @inline final def Try(body: Tree, cases: List[CaseDef], finalizer: Tree): Try = u.Try(body, cases, finalizer)

  @inline final def mkFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree = u.gen.mkFunctionTypeTree(argtpes, restpe)

  @inline final def emptyTree: Tree = u.EmptyTree

  @inline final def withPhaseNoLater[T](phase: Phase)(op: => T): T = u.enteringPhaseNotLaterThan[T](phase)(op)

  @inline final def mirrorThatLoaded(sym: Symbol): Mirror = u.mirrorThatLoaded(sym)

  @inline final def lub(tpe1: Type, tpe2: Type): Type = u.lub(tpe1 :: tpe2 :: Nil)
  @inline final def lub(tpes: List[Type]): Type = u.lub(tpes)

  @inline final def showRaw(tpe: Type): String = u.showRaw(tpe)

  @inline final def typeError[T](msg: String): T = throw new u.TypeError(msg)
}
