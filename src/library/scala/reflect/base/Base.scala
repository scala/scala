package scala.reflect
package base

import language.experimental.macros
import java.io.PrintWriter
import scala.annotation.switch
import scala.ref.WeakReference
import collection.mutable

class Base extends Universe { self =>

  private var nextId = 0

  abstract class Symbol(val name: Name, val flags: FlagSet) extends SymbolBase {
    val id = { nextId += 1; nextId }
    def owner: Symbol
    def fullName: String =
      if (isEffectiveRoot || owner.isEffectiveRoot) name.toString else owner.fullName + "." + name
    private def isEffectiveRoot =
      this == NoSymbol || this == rootMirror.RootClass || this == rootMirror.EmptyPackageClass

    def newTermSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol =
      new TermSymbol(this, name, flags)

    def newModuleAndClassSymbol(name: Name, pos: Position = NoPosition, flags: FlagSet = NoFlags): (ModuleSymbol, ClassSymbol) = {
      val c = new ModuleClassSymbol(this, name.toTypeName, flags)
      val m = new ModuleSymbol(this, name.toTermName, flags, c)
      (m, c)
    }

    def newMethodSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): MethodSymbol
      = new MethodSymbol(this, name, flags)

    def newTypeSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TypeSymbol =
      new TypeSymbol(this, name, flags)

    def newClassSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): ClassSymbol =
      new ClassSymbol(this, name, flags)

    def newFreeTermSymbol(name: TermName, info: Type, value: => Any, flags: FlagSet = NoFlags, origin: String = null) =
      new FreeTermSymbol(this, name, flags)

    def newFreeTypeSymbol(name: TypeName, info: Type, value: => Any, flags: FlagSet = NoFlags, origin: String = null) =
      new FreeTypeSymbol(this, name, flags)

    private def kindString: String =
      if (isModule) "module"
      else if (isClass) "class"
      else if (isFreeType) "free type"
      else if (isType) "type"
      else if (isMethod) "method"
      else if (isFreeTerm) "free term"
      else if (isTerm) "value"
      else "symbol"
    // [Eugene++ to Martin] base names should expose `decode`
    override def toString() = s"$kindString $name"
  }
  implicit val SymbolTag = ClassTag[Symbol](classOf[Symbol])

  class TermSymbol(val owner: Symbol, override val name: TermName, flags: FlagSet)
      extends Symbol(name, flags) with TermSymbolBase
  implicit val TermSymbolTag = ClassTag[TermSymbol](classOf[TermSymbol])

  class TypeSymbol(val owner: Symbol, override val name: TypeName, flags: FlagSet)
      extends Symbol(name, flags) with TypeSymbolBase {
    override val asTypeConstructor = TypeRef(ThisType(owner), this, Nil)
  }
  implicit val TypeSymbolTag = ClassTag[TypeSymbol](classOf[TypeSymbol])

  class MethodSymbol(owner: Symbol, name: TermName, flags: FlagSet)
      extends TermSymbol(owner, name, flags) with MethodSymbolBase
  implicit val MethodSymbolTag = ClassTag[MethodSymbol](classOf[MethodSymbol])

  class ModuleSymbol(owner: Symbol, name: TermName, flags: FlagSet, override val moduleClass: Symbol)
      extends TermSymbol(owner, name, flags) with ModuleSymbolBase
  implicit val ModuleSymbolTag = ClassTag[ModuleSymbol](classOf[ModuleSymbol])

  class ClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet)
      extends TypeSymbol(owner, name, flags) with ClassSymbolBase
  class ModuleClassSymbol(owner: Symbol, name: TypeName, flags: FlagSet)
      extends ClassSymbol(owner, name, flags) { override def isModuleClass = true }
  implicit val ClassSymbolTag = ClassTag[ClassSymbol](classOf[ClassSymbol])

  class FreeTermSymbol(owner: Symbol, name: TermName, flags: FlagSet)
      extends TermSymbol(owner, name, flags) with FreeTermSymbolBase
  implicit val FreeTermSymbolTag = ClassTag[FreeTermSymbol](classOf[FreeTermSymbol])

  class FreeTypeSymbol(owner: Symbol, name: TypeName, flags: FlagSet)
      extends TypeSymbol(owner, name, flags) with FreeTypeSymbolBase
  implicit val FreeTypeSymbolTag = ClassTag[FreeTypeSymbol](classOf[FreeTypeSymbol])


  object NoSymbol extends Symbol(nme.NO_NAME, NoFlags) {
    override def owner = throw new UnsupportedOperationException("NoSymbol.owner")
  }

  // todo. write a decent toString that doesn't crash on recursive types
  class Type extends TypeBase {
    def typeSymbol: Symbol = NoSymbol
    def termSymbol: Symbol = NoSymbol
  }
  implicit val TypeTagg = ClassTag[Type](classOf[Type])

  val NoType = new Type { override def toString = "NoType" }
  val NoPrefix = new Type { override def toString = "NoPrefix" }

  class SingletonType extends Type
  implicit val SingletonTypeTag = ClassTag[SingletonType](classOf[SingletonType])

  case class ThisType(sym: Symbol) extends SingletonType { override val typeSymbol = sym }
  object ThisType extends ThisTypeExtractor
  implicit val ThisTypeTag = ClassTag[ThisType](classOf[ThisType])

  case class SingleType(pre: Type, sym: Symbol) extends SingletonType { override val termSymbol = sym }
  object SingleType extends SingleTypeExtractor
  implicit val SingleTypeTag = ClassTag[SingleType](classOf[SingleType])

  case class SuperType(thistpe: Type, supertpe: Type) extends SingletonType
  object SuperType extends SuperTypeExtractor
  implicit val SuperTypeTag = ClassTag[SuperType](classOf[SuperType])

  case class ConstantType(value: Constant) extends SingletonType
  object ConstantType extends ConstantTypeExtractor
  implicit val ConstantTypeTag = ClassTag[ConstantType](classOf[ConstantType])

  case class TypeRef(pre: Type, sym: Symbol, args: List[Type]) extends Type { override val typeSymbol = sym }
  object TypeRef extends TypeRefExtractor
  implicit val TypeRefTag = ClassTag[TypeRef](classOf[TypeRef])

  abstract class CompoundType extends Type
  implicit val CompoundTypeTag = ClassTag[CompoundType](classOf[CompoundType])

  case class RefinedType(parents: List[Type], decls: Scope) extends CompoundType
  object RefinedType extends RefinedTypeExtractor {
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType =
      RefinedType(parents, decls)
  }
  implicit val RefinedTypeTag = ClassTag[RefinedType](classOf[RefinedType])

  case class ClassInfoType(parents: List[Type], decls: Scope, override val typeSymbol: Symbol) extends CompoundType
  object ClassInfoType extends ClassInfoTypeExtractor
  implicit val ClassInfoTypeTag = ClassTag[ClassInfoType](classOf[ClassInfoType])

  case class MethodType(params: List[Symbol], resultType: Type) extends Type
  object MethodType extends MethodTypeExtractor
  implicit val MethodTypeTag = ClassTag[MethodType](classOf[MethodType])

  case class NullaryMethodType(resultType: Type) extends Type
  object NullaryMethodType extends NullaryMethodTypeExtractor
  implicit val NullaryMethodTypeTag = ClassTag[NullaryMethodType](classOf[NullaryMethodType])

  case class PolyType(typeParams: List[Symbol], resultType: Type) extends Type
  object PolyType extends PolyTypeExtractor
  implicit val PolyTypeTag = ClassTag[PolyType](classOf[PolyType])

  case class ExistentialType(quantified: List[Symbol], underlying: Type) extends Type { override def typeSymbol = underlying.typeSymbol }
  object ExistentialType extends ExistentialTypeExtractor
  implicit val ExistentialTypeTag = ClassTag[ExistentialType](classOf[ExistentialType])

  case class AnnotatedType(annotations: List[AnnotationInfo], underlying: Type, selfsym: Symbol) extends Type { override def typeSymbol = underlying.typeSymbol }
  object AnnotatedType extends AnnotatedTypeExtractor
  implicit val AnnotatedTypeTag = ClassTag[AnnotatedType](classOf[AnnotatedType])

  case class TypeBounds(lo: Type, hi: Type) extends Type
  object TypeBounds extends TypeBoundsExtractor
  implicit val TypeBoundsTag = ClassTag[TypeBounds](classOf[TypeBounds])

  val WildcardType = new Type

  case class BoundedWildcardType(bounds: TypeBounds) extends Type
  object BoundedWildcardType extends BoundedWildcardTypeExtractor
  implicit val BoundedWildcardTypeTag = ClassTag[BoundedWildcardType](classOf[BoundedWildcardType])

  type Scope = Iterable[Symbol]
  implicit val ScopeTag = ClassTag[Scope](classOf[Scope])

  def newScope = newScopeWith()
  def newNestedScope(outer: Iterable[Symbol]) = newScope
  def newScopeWith(elems: Symbol*): Scope = elems

  abstract class Name(str: String) extends NameBase {
    override def toString = str
  }
  implicit val NameTag = ClassTag[Name](classOf[Name])

  class TermName(str: String) extends Name(str) {
    def isTermName = true
    def isTypeName = false
    def toTermName = this
    def toTypeName = new TypeName(str)
  }
  implicit val TermNameTag = ClassTag[TermName](classOf[TermName])

  class TypeName(str: String) extends Name(str) {
    def isTermName = false
    def isTypeName = true
    def toTermName = new TermName(str)
    def toTypeName = this
  }
  implicit val TypeNameTag = ClassTag[TypeName](classOf[TypeName])

  def newTermName(str: String) = new TermName(str)
  def newTypeName(str: String) = new TypeName(str)

  object nme extends TermNamesBase {
    type NameType = TermName
    val EMPTY              = newTermName("")
    val ROOT               = newTermName("<root>")
    val EMPTY_PACKAGE_NAME = newTermName("<empty>")
    val CONSTRUCTOR        = newTermName("<init>")
    val NO_NAME            = newTermName("<none>")
    val WILDCARD           = newTermName("_")
  }

  object tpnme extends TypeNamesBase {
    type NameType = TypeName
    val EMPTY              = nme.EMPTY.toTypeName
    val ROOT               = nme.ROOT.toTypeName
    val EMPTY_PACKAGE_NAME = nme.EMPTY_PACKAGE_NAME.toTypeName
    val WILDCARD           = nme.WILDCARD.toTypeName
  }

  type FlagSet = Long
  val NoFlags = 0L
  implicit val FlagSetTag = ClassTag[FlagSet](classOf[FlagSet])

  class Modifiers(override val flags: FlagSet,
                  override val privateWithin: Name,
                  override val annotations: List[Tree]) extends ModifiersBase {
    def hasFlag(flags: FlagSet) = (this.flags & flags) != 0
    def hasAllFlags(flags: FlagSet) = (flags & ~this.flags) == 0
  }

  implicit val ModifiersTag = ClassTag[Modifiers](classOf[Modifiers])

  object Modifiers extends ModifiersCreator {
    def apply(flags: Long,
              privateWithin: Name,
              annotations: List[Tree]) = new Modifiers(flags, privateWithin, annotations)
  }

  case class Constant(value: Any)
  object Constant extends ConstantExtractor
  implicit val ConstantTag = ClassTag[Constant](classOf[Constant])

  case class AnnotationInfo(atp: Type, args: List[Tree], assocs: List[(Name, ClassfileAnnotArg)])
  object AnnotationInfo extends AnnotationInfoExtractor
  implicit val AnnotationInfoTag = ClassTag[AnnotationInfo](classOf[AnnotationInfo])

  abstract class ClassfileAnnotArg
  implicit val ClassfileAnnotArgTag = ClassTag[ClassfileAnnotArg](classOf[ClassfileAnnotArg])

  case class LiteralAnnotArg(const: Constant) extends ClassfileAnnotArg
  object LiteralAnnotArg extends LiteralAnnotArgExtractor
  implicit val LiteralAnnotArgTag = ClassTag[LiteralAnnotArg](classOf[LiteralAnnotArg])

  case class ArrayAnnotArg(args: Array[ClassfileAnnotArg]) extends ClassfileAnnotArg
  object ArrayAnnotArg extends ArrayAnnotArgExtractor
  implicit val ArrayAnnotArgTag = ClassTag[ArrayAnnotArg](classOf[ArrayAnnotArg])

  case class NestedAnnotArg(annInfo: AnnotationInfo) extends ClassfileAnnotArg
  object NestedAnnotArg extends NestedAnnotArgExtractor
  implicit val NestedAnnotArgTag = ClassTag[NestedAnnotArg](classOf[NestedAnnotArg])

  class Position extends Attachments {
    override type Pos = Position
    def pos = this
    def withPos(newPos: Position) = newPos
    def isRange = false
    def focus = this
  }
  implicit val PositionTag = ClassTag[Position](classOf[Position])

  val NoPosition = new Position

  def atPos[T <: Tree](pos: Position)(tree: T): T = tree

  private val generated = new mutable.HashMap[String, WeakReference[Symbol]]

  private def cached(name: String)(symExpr: => Symbol): Symbol =
    generated get name match {
      case Some(WeakReference(sym)) =>
        sym
      case _ =>
        val sym = symExpr
        generated(name) = WeakReference(sym)
        sym
    }

  object build extends BuildBase {
    def selectType(owner: Symbol, name: String): TypeSymbol = {
      val clazz = new ClassSymbol(owner, newTypeName(name), NoFlags)
      cached(clazz.fullName)(clazz).asTypeSymbol
    }

    def selectTerm(owner: Symbol, name: String): TermSymbol = {
      val valu = new MethodSymbol(owner, newTermName(name), NoFlags)
      cached(valu.fullName)(valu).asTermSymbol
    }

    def selectOverloadedMethod(owner: Symbol, name: String, index: Int): MethodSymbol =
      selectTerm(owner, name).asMethodSymbol

    def newNestedSymbol(owner: Symbol, name: Name, pos: Position, flags: Long, isClass: Boolean): Symbol =
      if (name.isTypeName)
        if (isClass) new ClassSymbol(owner, name.toTypeName, flags)
        else new TypeSymbol(owner, name.toTypeName, flags)
      else new TermSymbol(owner, name.toTermName, flags)

    def newFreeTerm(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTermSymbol =
      new FreeTermSymbol(rootMirror.RootClass, newTermName(name), flags)

    def newFreeType(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
      new FreeTypeSymbol(rootMirror.RootClass, newTypeName(name), flags)

    def newFreeExistential(name: String, info: Type, value: => Any, flags: Long = 0L, origin: String = null): FreeTypeSymbol =
      new FreeTypeSymbol(rootMirror.RootClass, newTypeName(name), flags)

    def setTypeSignature[S <: Symbol](sym: S, tpe: Type): S = sym

    def setAnnotations[S <: Symbol](sym: S, annots: List[AnnotationInfo]): S = sym

    def flagsFromBits(bits: Long): FlagSet = bits

    object emptyValDef extends ValDef(NoMods, nme.WILDCARD, TypeTree(NoType), EmptyTree) {
      override def isEmpty = true
    }

    def This(sym: Symbol): Tree = self.This(sym.name.toTypeName)

    def Select(qualifier: Tree, sym: Symbol): Select = self.Select(qualifier, sym.name)

    def Ident(sym: Symbol): Ident = self.Ident(sym.name)

    def TypeTree(tp: Type): TypeTree = self.TypeTree()

    def thisPrefix(sym: Symbol): Type = SingleType(NoPrefix, sym)

    def setType[T <: Tree](tree: T, tpe: Type): T = tree

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T = tree
  }

  import build._

  class Mirror extends MirrorOf[self.type] {
    val universe: self.type = self

    lazy val RootClass    = new ClassSymbol(NoSymbol, tpnme.ROOT, NoFlags) { override def isModuleClass = true }
    lazy val RootPackage  = new ModuleSymbol(NoSymbol, nme.ROOT, NoFlags, RootClass)
    lazy val EmptyPackageClass = new ClassSymbol(RootClass, tpnme.EMPTY_PACKAGE_NAME, NoFlags) { override def isModuleClass = true }
    lazy val EmptyPackage = new ModuleSymbol(RootClass, nme.EMPTY_PACKAGE_NAME, NoFlags, EmptyPackageClass)

    def staticClass(fullName: String): ClassSymbol =
      mkStatic[ClassSymbol](fullName)

    def staticModule(fullName: String): ModuleSymbol =
      mkStatic[ModuleSymbol](fullName)

    private def mkStatic[S <: Symbol : ClassTag](fullName: String): S =
      cached(fullName) {
        val point = fullName lastIndexOf '.'
        val owner =
          if (point > 0) staticModule(fullName take point).moduleClass
          else rootMirror.RootClass
        val name = fullName drop point + 1
        val symtag = implicitly[ClassTag[S]]
        if (symtag == ClassSymbolTag) new ClassSymbol(owner, newTypeName(name), NoFlags)
        else owner.newModuleAndClassSymbol(newTermName(name))._1
      }.asInstanceOf[S]
  }

  lazy val rootMirror = new Mirror

  import rootMirror._

  object definitions extends DefinitionsBase {
    lazy val ScalaPackage = staticModule("scala")
    lazy val ScalaPackageClass = ScalaPackage.moduleClass.asClassSymbol

    lazy val AnyClass     = staticClass("scala.Any")
    lazy val AnyValClass  = staticClass("scala.Any")
    lazy val ObjectClass  = staticClass("java.lang.Object")
    lazy val AnyRefClass  = ObjectClass

    lazy val NullClass    = staticClass("scala.Null")
    lazy val NothingClass = staticClass("scala.Nothing")

    lazy val UnitClass    = staticClass("scala.Unit")
    lazy val ByteClass    = staticClass("scala.Byte")
    lazy val ShortClass   = staticClass("scala.Short")
    lazy val CharClass    = staticClass("scala.Char")
    lazy val IntClass     = staticClass("scala.Int")
    lazy val LongClass    = staticClass("scala.Long")
    lazy val FloatClass   = staticClass("scala.Float")
    lazy val DoubleClass  = staticClass("scala.Double")
    lazy val BooleanClass = staticClass("scala.Boolean")

    lazy val StringClass  = staticClass("java.lang.String")
    lazy val ClassClass   = staticClass("java.lang.Class")
    lazy val ArrayClass   = staticClass("scala.Array")
    lazy val ListClass    = staticClass("scala.List")

    lazy val PredefModule = staticModule("scala.Predef")
  }

  import definitions._

  private def thisModuleType(fullName: String): Type = ThisType(staticModule(fullName).moduleClass)
  private lazy val ScalaPrefix = thisModuleType("scala")
  private lazy val JavaLangPrefix = thisModuleType("java.lang")

  lazy val ByteTpe    = TypeRef(ScalaPrefix, ByteClass, Nil)
  lazy val ShortTpe   = TypeRef(ScalaPrefix, ShortClass, Nil)
  lazy val CharTpe    = TypeRef(ScalaPrefix, CharClass, Nil)
  lazy val IntTpe     = TypeRef(ScalaPrefix, IntClass, Nil)
  lazy val LongTpe    = TypeRef(ScalaPrefix, LongClass, Nil)
  lazy val FloatTpe   = TypeRef(ScalaPrefix, FloatClass, Nil)
  lazy val DoubleTpe  = TypeRef(ScalaPrefix, DoubleClass, Nil)
  lazy val BooleanTpe = TypeRef(ScalaPrefix, BooleanClass, Nil)
  lazy val UnitTpe    = TypeRef(ScalaPrefix, UnitClass, Nil)
  lazy val AnyTpe     = TypeRef(ScalaPrefix, AnyClass, Nil)
  lazy val AnyValTpe  = TypeRef(ScalaPrefix, AnyValClass, Nil)
  lazy val NothingTpe = TypeRef(ScalaPrefix, NothingClass, Nil)
  lazy val NullTpe    = TypeRef(ScalaPrefix, NullClass, Nil)
  lazy val ObjectTpe  = TypeRef(JavaLangPrefix, ObjectClass, Nil)
  lazy val AnyRefTpe  = ObjectTpe

  private var nodeCount = 0 // not synchronized

  abstract class Tree extends TreeBase with Product {
    def isDef: Boolean = false
    def isEmpty: Boolean = false

    /** The canonical way to test if a Tree represents a term.
     */
    def isTerm: Boolean = this match {
      case _: TermTree       => true
      case Bind(name, _)     => name.isTermName
      case Select(_, name)   => name.isTermName
      case Ident(name)       => name.isTermName
      case Annotated(_, arg) => arg.isTerm
      case _                 => false
    }

    /** The canonical way to test if a Tree represents a type.
     */
    def isType: Boolean = this match {
      case _: TypTree        => true
      case Bind(name, _)     => name.isTypeName
      case Select(_, name)   => name.isTypeName
      case Ident(name)       => name.isTypeName
      case Annotated(_, arg) => arg.isType
      case _                 => false
    }
  }

  def treeToString(tree: Tree) = s"<tree ${tree.getClass}>"

  trait TermTree extends Tree

  trait TypTree extends Tree

  trait SymTree extends Tree

  trait NameTree extends Tree {
    def name: Name
  }

  trait RefTree extends SymTree with NameTree {
    def qualifier: Tree    // empty for Idents
    def name: Name
  }

  abstract class DefTree extends SymTree with NameTree {
    def name: Name
    override def isDef = true
  }

  case object EmptyTree extends TermTree {
    override def isEmpty = true
  }

  abstract class MemberDef extends DefTree {
    def mods: Modifiers
  }

  case class PackageDef(pid: RefTree, stats: List[Tree])
       extends MemberDef {
    def name = pid.name
    def mods = NoMods
  }
  object PackageDef extends PackageDefExtractor

  abstract class ImplDef extends MemberDef {
    def impl: Template
  }

  case class ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template)
       extends ImplDef
  object ClassDef extends ClassDefExtractor

  case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)
        extends ImplDef
  object ModuleDef extends ModuleDefExtractor

  abstract class ValOrDefDef extends MemberDef {
    val name: Name
    val tpt: Tree
    val rhs: Tree
  }

  case class ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree) extends ValOrDefDef
  object ValDef extends ValDefExtractor

  case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef],
                    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) extends ValOrDefDef
  object DefDef extends DefDefExtractor

  case class TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree)
       extends MemberDef
  object TypeDef extends TypeDefExtractor

  case class LabelDef(name: TermName, params: List[Ident], rhs: Tree)
       extends DefTree with TermTree
  object LabelDef extends LabelDefExtractor

  case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int)
  object ImportSelector extends ImportSelectorExtractor

  case class Import(expr: Tree, selectors: List[ImportSelector])
       extends SymTree
  object Import extends ImportExtractor

  case class Template(parents: List[Tree], self: ValDef, body: List[Tree])
       extends SymTree
  object Template extends TemplateExtractor

  case class Block(stats: List[Tree], expr: Tree)
       extends TermTree
  object Block extends BlockExtractor

  case class CaseDef(pat: Tree, guard: Tree, body: Tree)
       extends Tree
  object CaseDef extends CaseDefExtractor

  case class Alternative(trees: List[Tree])
       extends TermTree
  object Alternative extends AlternativeExtractor

  case class Star(elem: Tree)
       extends TermTree
  object Star extends StarExtractor

  case class Bind(name: Name, body: Tree)
       extends DefTree
  object Bind extends BindExtractor

  case class UnApply(fun: Tree, args: List[Tree])
       extends TermTree
  object UnApply extends UnApplyExtractor

  case class ArrayValue(elemtpt: Tree, elems: List[Tree])
       extends TermTree
  object ArrayValue extends ArrayValueExtractor

  case class Function(vparams: List[ValDef], body: Tree)
       extends TermTree with SymTree
  object Function extends FunctionExtractor

  case class Assign(lhs: Tree, rhs: Tree)
       extends TermTree
  object Assign extends AssignExtractor

  case class AssignOrNamedArg(lhs: Tree, rhs: Tree)
       extends TermTree
  object AssignOrNamedArg extends AssignOrNamedArgExtractor

  case class If(cond: Tree, thenp: Tree, elsep: Tree)
       extends TermTree
  object If extends IfExtractor

  case class Match(selector: Tree, cases: List[CaseDef])
       extends TermTree
  object Match extends MatchExtractor

  case class Return(expr: Tree)
       extends TermTree with SymTree
  object Return extends ReturnExtractor

  case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)
       extends TermTree
  object Try extends TryExtractor

  case class Throw(expr: Tree)
       extends TermTree
  object Throw extends ThrowExtractor

  case class New(tpt: Tree) extends TermTree
  object New extends NewExtractor

  case class Typed(expr: Tree, tpt: Tree)
       extends TermTree
  object Typed extends TypedExtractor

  abstract class GenericApply extends TermTree {
    val fun: Tree
    val args: List[Tree]
  }

  case class TypeApply(fun: Tree, args: List[Tree])
       extends GenericApply
  object TypeApply extends TypeApplyExtractor

  case class Apply(fun: Tree, args: List[Tree])
       extends GenericApply
  object Apply extends ApplyExtractor

  case class ApplyDynamic(qual: Tree, args: List[Tree])
       extends TermTree with SymTree
  object ApplyDynamic extends ApplyDynamicExtractor

  case class Super(qual: Tree, mix: TypeName) extends TermTree
  object Super extends SuperExtractor

  case class This(qual: TypeName)
        extends TermTree with SymTree
  object This extends ThisExtractor

  case class Select(qualifier: Tree, name: Name)
       extends RefTree
  object Select extends SelectExtractor

  case class Ident(name: Name) extends RefTree {
    def qualifier: Tree = EmptyTree
  }
  object Ident extends IdentExtractor

  case class ReferenceToBoxed(ident: Ident) extends TermTree
  object ReferenceToBoxed extends ReferenceToBoxedExtractor

  case class Literal(value: Constant)
        extends TermTree {
    assert(value ne null)
  }
  object Literal extends LiteralExtractor

  case class Annotated(annot: Tree, arg: Tree) extends Tree
  object Annotated extends AnnotatedExtractor

  case class SingletonTypeTree(ref: Tree)
        extends TypTree
  object SingletonTypeTree extends SingletonTypeTreeExtractor

  case class SelectFromTypeTree(qualifier: Tree, name: TypeName)
       extends TypTree with RefTree
  object SelectFromTypeTree extends SelectFromTypeTreeExtractor

  case class CompoundTypeTree(templ: Template)
       extends TypTree
  object CompoundTypeTree extends CompoundTypeTreeExtractor

  case class AppliedTypeTree(tpt: Tree, args: List[Tree])
       extends TypTree
  object AppliedTypeTree extends AppliedTypeTreeExtractor

  case class TypeBoundsTree(lo: Tree, hi: Tree)
       extends TypTree
  object TypeBoundsTree extends TypeBoundsTreeExtractor

  case class ExistentialTypeTree(tpt: Tree, whereClauses: List[Tree])
       extends TypTree
  object ExistentialTypeTree extends ExistentialTypeTreeExtractor

  case class TypeTree() extends TypTree {
    val original: Tree = null
    override def isEmpty = true
  }
  object TypeTree extends TypeTreeExtractor

  implicit val TreeTag = ClassTag[Tree](classOf[Tree])
  implicit val TermTreeTag = ClassTag[TermTree](classOf[TermTree])
  implicit val TypTreeTag = ClassTag[TypTree](classOf[TypTree])
  implicit val SymTreeTag = ClassTag[SymTree](classOf[SymTree])
  implicit val NameTreeTag = ClassTag[NameTree](classOf[NameTree])
  implicit val RefTreeTag = ClassTag[RefTree](classOf[RefTree])
  implicit val DefTreeTag = ClassTag[DefTree](classOf[DefTree])
  implicit val MemberDefTag = ClassTag[MemberDef](classOf[MemberDef])
  implicit val PackageDefTag = ClassTag[PackageDef](classOf[PackageDef])
  implicit val ImplDefTag = ClassTag[ImplDef](classOf[ImplDef])
  implicit val ClassDefTag = ClassTag[ClassDef](classOf[ClassDef])
  implicit val ModuleDefTag = ClassTag[ModuleDef](classOf[ModuleDef])
  implicit val ValOrDefDefTag = ClassTag[ValOrDefDef](classOf[ValOrDefDef])
  implicit val ValDefTag = ClassTag[ValDef](classOf[ValDef])
  implicit val DefDefTag = ClassTag[DefDef](classOf[DefDef])
  implicit val TypeDefTag = ClassTag[TypeDef](classOf[TypeDef])
  implicit val LabelDefTag = ClassTag[LabelDef](classOf[LabelDef])
  implicit val ImportSelectorTag = ClassTag[ImportSelector](classOf[ImportSelector])
  implicit val ImportTag = ClassTag[Import](classOf[Import])
  implicit val TemplateTag = ClassTag[Template](classOf[Template])
  implicit val BlockTag = ClassTag[Block](classOf[Block])
  implicit val CaseDefTag = ClassTag[CaseDef](classOf[CaseDef])
  implicit val AlternativeTag = ClassTag[Alternative](classOf[Alternative])
  implicit val StarTag = ClassTag[Star](classOf[Star])
  implicit val BindTag = ClassTag[Bind](classOf[Bind])
  implicit val UnApplyTag = ClassTag[UnApply](classOf[UnApply])
  implicit val ArrayValueTag = ClassTag[ArrayValue](classOf[ArrayValue])
  implicit val FunctionTag = ClassTag[Function](classOf[Function])
  implicit val AssignTag = ClassTag[Assign](classOf[Assign])
  implicit val AssignOrNamedArgTag = ClassTag[AssignOrNamedArg](classOf[AssignOrNamedArg])
  implicit val IfTag = ClassTag[If](classOf[If])
  implicit val MatchTag = ClassTag[Match](classOf[Match])
  implicit val ReturnTag = ClassTag[Return](classOf[Return])
  implicit val TryTag = ClassTag[Try](classOf[Try])
  implicit val ThrowTag = ClassTag[Throw](classOf[Throw])
  implicit val NewTag = ClassTag[New](classOf[New])
  implicit val TypedTag = ClassTag[Typed](classOf[Typed])
  implicit val GenericApplyTag = ClassTag[GenericApply](classOf[GenericApply])
  implicit val TypeApplyTag = ClassTag[TypeApply](classOf[TypeApply])
  implicit val ApplyTag = ClassTag[Apply](classOf[Apply])
  implicit val ApplyDynamicTag = ClassTag[ApplyDynamic](classOf[ApplyDynamic])
  implicit val SuperTag = ClassTag[Super](classOf[Super])
  implicit val ThisTag = ClassTag[This](classOf[This])
  implicit val SelectTag = ClassTag[Select](classOf[Select])
  implicit val IdentTag = ClassTag[Ident](classOf[Ident])
  implicit val ReferenceToBoxedTag = ClassTag[ReferenceToBoxed](classOf[ReferenceToBoxed])
  implicit val LiteralTag = ClassTag[Literal](classOf[Literal])
  implicit val AnnotatedTag = ClassTag[Annotated](classOf[Annotated])
  implicit val SingletonTypeTreeTag = ClassTag[SingletonTypeTree](classOf[SingletonTypeTree])
  implicit val SelectFromTypeTreeTag = ClassTag[SelectFromTypeTree](classOf[SelectFromTypeTree])
  implicit val CompoundTypeTreeTag = ClassTag[CompoundTypeTree](classOf[CompoundTypeTree])
  implicit val AppliedTypeTreeTag = ClassTag[AppliedTypeTree](classOf[AppliedTypeTree])
  implicit val TypeBoundsTreeTag = ClassTag[TypeBoundsTree](classOf[TypeBoundsTree])
  implicit val ExistentialTypeTreeTag = ClassTag[ExistentialTypeTree](classOf[ExistentialTypeTree])
  implicit val TypeTreeTag = ClassTag[TypeTree](classOf[TypeTree])

  // [Eugene++] to be removed after SI-5863 is fixed
  def ClassDef(sym: Symbol, impl: Template): ClassDef = ???
  def ModuleDef(sym: Symbol, impl: Template): ModuleDef = ???
  def ValDef(sym: Symbol, rhs: Tree): ValDef = ???
  def ValDef(sym: Symbol): ValDef = ???
  def DefDef(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef = ???
  def DefDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef = ???
  def DefDef(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef = ???
  def DefDef(sym: Symbol, rhs: Tree): DefDef = ???
  def DefDef(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef = ???
  def TypeDef(sym: Symbol, rhs: Tree): TypeDef = ???
  def TypeDef(sym: Symbol): TypeDef = ???
  def LabelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef = ???
  def CaseDef(pat: Tree, body: Tree): CaseDef = ???
  def Bind(sym: Symbol, body: Tree): Bind = ???
  def Try(body: Tree, cases: (Tree, Tree)*): Try = ???
  def Throw(tpe: Type, args: Tree*): Throw = ???
  def Apply(sym: Symbol, args: Tree*): Tree = ???
  def New(tpt: Tree, argss: List[List[Tree]]): Tree = ???
  def New(tpe: Type, args: Tree*): Tree = ???
  def New(sym: Symbol, args: Tree*): Tree = ???
  def ApplyConstructor(tpt: Tree, args: List[Tree]): Tree = ???
  def Super(sym: Symbol, mix: TypeName): Tree = ???
  def This(sym: Symbol): Tree = ???
  def Select(qualifier: Tree, name: String): Select = ???
  def Select(qualifier: Tree, sym: Symbol): Select = ???
  def Ident(name: String): Ident = ???
  def Ident(sym: Symbol): Ident = ???
  def Block(stats: Tree*): Block = ???
  def TypeTree(tp: Type): TypeTree = ???
}
