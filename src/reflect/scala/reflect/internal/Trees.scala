/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

import Flags._
import base.Attachments
import collection.mutable.{ListBuffer, LinkedHashSet}
import util.Statistics

trait Trees extends api.Trees { self: SymbolTable =>

  private[scala] var nodeCount = 0

  abstract class Tree extends TreeContextApiImpl with Attachable with Product {
    val id = nodeCount // TODO: add to attachment?
    nodeCount += 1

    Statistics.incCounter(TreesStats.nodeByType, getClass)

    @inline final override def pos: Position = rawatt.pos

    private[this] var rawtpe: Type = _
    @inline final def tpe = rawtpe
    def tpe_=(t: Type) = rawtpe = t
    def setType(tp: Type): this.type = { rawtpe = tp; this }
    def defineType(tp: Type): this.type = setType(tp)

    def symbol: Symbol = null
    def symbol_=(sym: Symbol) { throw new UnsupportedOperationException("symbol_= inapplicable for " + this) }
    def setSymbol(sym: Symbol): this.type = { symbol = sym; this }
    def hasSymbol = false

    def isDef = false

    def isEmpty = false

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

    private[scala] def copyAttrs(tree: Tree): this.type = {
      rawatt = tree.rawatt
      tpe = tree.tpe
      if (hasSymbol) symbol = tree.symbol
      this
    }

    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

    override def duplicate: this.type =
      (duplicator transform this).asInstanceOf[this.type]
  }

  abstract class TreeContextApiImpl extends TreeContextApi { this: Tree =>

    override def orElse(alt: => Tree) = if (!isEmpty) this else alt

    override def foreach(f: Tree => Unit) { new ForeachTreeTraverser(f).traverse(this) }

    override def withFilter(f: Tree => Boolean): List[Tree] = {
      val ft = new FilterTreeTraverser(f)
      ft.traverse(this)
      ft.hits.toList
    }

    override def filter(f: Tree => Boolean): List[Tree] = withFilter(f)

    override def collect[T](pf: PartialFunction[Tree, T]): List[T] = {
      val ctt = new CollectTreeTraverser[T](pf)
      ctt.traverse(this)
      ctt.results.toList
    }

    override def find(p: Tree => Boolean): Option[Tree] = {
      val ft = new FindTreeTraverser(p)
      ft.traverse(this)
      ft.result
    }

    override def exists(p: Tree => Boolean): Boolean = !find(p).isEmpty

    override def forAll(p: Tree => Boolean): Boolean = find(!p(_)).isEmpty

    override def equalsStructure(that : Tree) = correspondsStructure(that)(_ eq _)

    def correspondsStructure(that: Tree)(f: (Tree,Tree) => Boolean): Boolean =
      f(this, that) || ((productArity == that.productArity) && {
        def equals0(this0: Any, that0: Any): Boolean = (this0, that0) match {
          case (x: Tree, y: Tree)         => f(x, y) || (x correspondsStructure y)(f)
          case (xs: List[_], ys: List[_]) => (xs corresponds ys)(equals0)
          case _                          => this0 == that0
        }
        def compareOriginals() = (this, that) match {
          case (x: TypeTree, y: TypeTree) if x.original != null && y.original != null =>
            (x.original correspondsStructure y.original)(f)
          case _                          =>
            true
        }

        (productIterator zip that.productIterator forall { case (x, y) => equals0(x, y) }) && compareOriginals()
      })

    override def children: List[Tree] = {
      def subtrees(x: Any): List[Tree] = x match {
        case EmptyTree   => Nil
        case t: Tree     => List(t)
        case xs: List[_] => xs flatMap subtrees
        case _           => Nil
      }
      productIterator.toList flatMap subtrees
    }

    override def freeTerms: List[FreeTermSymbol] = freeSyms[FreeTermSymbol](_.isFreeTerm, _.termSymbol)
    override def freeTypes: List[FreeTypeSymbol] = freeSyms[FreeTypeSymbol](_.isFreeType, _.typeSymbol)

    private def freeSyms[S <: Symbol](isFree: Symbol => Boolean, symOfType: Type => Symbol): List[S] = {
      val s = collection.mutable.LinkedHashSet[S]()
      def addIfFree(sym: Symbol): Unit = if (sym != null && isFree(sym)) s += sym.asInstanceOf[S]
      for (t <- this) {
        addIfFree(t.symbol)
        if (t.tpe != null) {
          for (tp <- t.tpe) {
            addIfFree(symOfType(tp))
          }
        }
      }
      s.toList
    }

    override def substituteSymbols(from: List[Symbol], to: List[Symbol]): Tree =
      new TreeSymSubstituter(from, to)(this)

    override def substituteTypes(from: List[Symbol], to: List[Type]): Tree =
      new TreeTypeSubstituter(from, to)(this)

    override def substituteThis(clazz: Symbol, to: Tree): Tree =
      new ThisSubstituter(clazz, to) transform this

    def hasSymbolWhich(f: Symbol => Boolean) =
      hasSymbol && symbol != null && f(symbol)

    def isErroneous = (tpe ne null) && tpe.isErroneous
    def isTyped     = (tpe ne null) && !tpe.isErroneous

    /** Sets the tree's type to the result of the given function.
     *  If the type is null, it remains null - the function is not called.
     */
    def modifyType(f: Type => Type): Tree =
      if (tpe eq null) this
      else this setType f(tpe)

    /** If `pf` is defined for a given subtree, call super.traverse(pf(tree)),
     *  otherwise super.traverse(tree).
     */
    def foreachPartial(pf: PartialFunction[Tree, Tree]) {
      new ForeachPartialTreeTraverser(pf).traverse(this)
    }

    def changeOwner(pairs: (Symbol, Symbol)*): Tree = {
      pairs.foldLeft(this) { case (t, (oldOwner, newOwner)) =>
        new ChangeOwnerTraverser(oldOwner, newOwner) apply t
      }
    }

    def shallowDuplicate: Tree = new ShallowDuplicator(this) transform this
    def shortClass: String = (getClass.getName split "[.$]").last

    def isErrorTyped = (tpe ne null) && tpe.isError

    /** When you want to know a little more than the class, but a lot
     *  less than the whole tree.
     */
    def summaryString: String = this match {
      case Literal(const)     => "Literal(" + const + ")"
      case Ident(name)        => "Ident(%s)".format(name.decode)
      case Select(qual, name) => "Select(%s, %s)".format(qual.summaryString, name.decode)
      case t: NameTree        => t.name.longString
      case t                  =>
        t.shortClass + (
          if (t.symbol != null && t.symbol != NoSymbol) "(" + t.symbol + ")"
          else ""
        )
    }
  }

  trait TermTree extends Tree with TermTreeApi

  trait TypTree extends Tree with TypTreeApi

  trait SymTree extends Tree with SymTreeContextApi {
    override def hasSymbol = true
    override var symbol: Symbol = NoSymbol
  }

  trait NameTree extends Tree with NameTreeApi {
    def name: Name
  }

  trait RefTree extends SymTree with NameTree with RefTreeApi {
    def qualifier: Tree    // empty for Idents
    def name: Name
  }

  abstract class DefTree extends SymTree with NameTree with DefTreeApi {
    def name: Name
    override def isDef = true
  }

  case object EmptyTree extends TermTree {
    super.tpe_=(NoType)
    override def tpe_=(t: Type) =
      if (t != NoType) throw new UnsupportedOperationException("tpe_=("+t+") inapplicable for <empty>")
    override def isEmpty = true
  }

  abstract class MemberDef extends DefTree with MemberDefApi {
    def mods: Modifiers
    def keyword: String = this match {
      case TypeDef(_, _, _, _)      => "type"
      case ClassDef(mods, _, _, _)  => if (mods hasFlag TRAIT) "trait" else "class"
      case DefDef(_, _, _, _, _, _) => "def"
      case ModuleDef(_, _, _)       => "object"
      case PackageDef(_, _)         => "package"
      case ValDef(mods, _, _, _)    => if (mods hasFlag MUTABLE) "var" else "val"
      case _ => ""
    }
  }

  case class PackageDef(pid: RefTree, stats: List[Tree])
       extends MemberDef with PackageDefApi {
    def name = pid.name
    def mods = NoMods
  }
  object PackageDef extends PackageDefExtractor

  abstract class ImplDef extends MemberDef with ImplDefApi {
    def impl: Template
  }

  case class ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template)
       extends ImplDef with ClassDefApi
  object ClassDef extends ClassDefExtractor

  case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)
        extends ImplDef with ModuleDefApi
  object ModuleDef extends ModuleDefExtractor

  abstract class ValOrDefDef extends MemberDef with ValOrDefDefApi {
    def name: Name
    def tpt: Tree
    def rhs: Tree
  }

  case class ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree) extends ValOrDefDef with ValDefApi
  object ValDef extends ValDefExtractor

  case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef],
                    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) extends ValOrDefDef with DefDefApi
  object DefDef extends DefDefExtractor

  case class TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree)
       extends MemberDef with TypeDefApi
  object TypeDef extends TypeDefExtractor

  case class LabelDef(name: TermName, params: List[Ident], rhs: Tree)
       extends DefTree with TermTree with LabelDefApi
  object LabelDef extends LabelDefExtractor

  case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int) extends ImportSelectorApi
  object ImportSelector extends ImportSelectorExtractor

  case class Import(expr: Tree, selectors: List[ImportSelector])
       extends SymTree with ImportApi
  object Import extends ImportExtractor

  case class Template(parents: List[Tree], self: ValDef, body: List[Tree])
       extends SymTree with TemplateApi
  object Template extends TemplateExtractor

  case class Block(stats: List[Tree], expr: Tree)
       extends TermTree with BlockApi
  object Block extends BlockExtractor

  case class CaseDef(pat: Tree, guard: Tree, body: Tree)
       extends Tree with CaseDefApi
  object CaseDef extends CaseDefExtractor

  case class Alternative(trees: List[Tree])
       extends TermTree with AlternativeApi
  object Alternative extends AlternativeExtractor

  case class Star(elem: Tree)
       extends TermTree with StarApi
  object Star extends StarExtractor

  case class Bind(name: Name, body: Tree)
       extends DefTree with BindApi
  object Bind extends BindExtractor

  case class UnApply(fun: Tree, args: List[Tree])
       extends TermTree with UnApplyApi
  object UnApply extends UnApplyExtractor

  case class ArrayValue(elemtpt: Tree, elems: List[Tree])
       extends TermTree with ArrayValueApi
  object ArrayValue extends ArrayValueExtractor

  case class Function(vparams: List[ValDef], body: Tree)
       extends TermTree with SymTree with FunctionApi
  object Function extends FunctionExtractor

  case class Assign(lhs: Tree, rhs: Tree)
       extends TermTree with AssignApi
  object Assign extends AssignExtractor

  case class AssignOrNamedArg(lhs: Tree, rhs: Tree)
       extends TermTree with AssignOrNamedArgApi
  object AssignOrNamedArg extends AssignOrNamedArgExtractor

  case class If(cond: Tree, thenp: Tree, elsep: Tree)
       extends TermTree with IfApi
  object If extends IfExtractor

  case class Match(selector: Tree, cases: List[CaseDef])
       extends TermTree with MatchApi
  object Match extends MatchExtractor

  case class Return(expr: Tree)
       extends TermTree with SymTree with ReturnApi
  object Return extends ReturnExtractor

  case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)
       extends TermTree with TryApi
  object Try extends TryExtractor

  case class Throw(expr: Tree)
       extends TermTree with ThrowApi
  object Throw extends ThrowExtractor

  case class New(tpt: Tree) extends TermTree with NewApi
  object New extends NewExtractor

  case class Typed(expr: Tree, tpt: Tree)
       extends TermTree with TypedApi
  object Typed extends TypedExtractor

  abstract class GenericApply extends TermTree with GenericApplyApi {
    val fun: Tree
    val args: List[Tree]
  }

  case class TypeApply(fun: Tree, args: List[Tree])
       extends GenericApply with TypeApplyApi {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol) { fun.symbol = sym }
  }
  object TypeApply extends TypeApplyExtractor

  case class Apply(fun: Tree, args: List[Tree])
       extends GenericApply with ApplyApi {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol) { fun.symbol = sym }
  }
  object Apply extends ApplyExtractor

  // TODO remove this class, add a tree attachment to Apply to track whether implicits were involved
  // copying trees will all too easily forget to distinguish subclasses
  class ApplyToImplicitArgs(fun: Tree, args: List[Tree]) extends Apply(fun, args)

  // TODO remove this class, add a tree attachment to Apply to track whether implicits were involved
  // copying trees will all too easily forget to distinguish subclasses
  class ApplyImplicitView(fun: Tree, args: List[Tree]) extends Apply(fun, args)

  def ApplyConstructor(tpt: Tree, args: List[Tree]) = Apply(Select(New(tpt), nme.CONSTRUCTOR), args)

  case class ApplyDynamic(qual: Tree, args: List[Tree])
       extends TermTree with SymTree with ApplyDynamicApi
  object ApplyDynamic extends ApplyDynamicExtractor

  case class Super(qual: Tree, mix: TypeName) extends TermTree with SuperApi {
    override def symbol: Symbol = qual.symbol
    override def symbol_=(sym: Symbol) { qual.symbol = sym }
  }
  object Super extends SuperExtractor

  case class This(qual: TypeName)
        extends TermTree with SymTree with ThisApi
  object This extends ThisExtractor

  case class Select(qualifier: Tree, name: Name)
       extends RefTree with SelectApi
  object Select extends SelectExtractor

  case class Ident(name: Name) extends RefTree with IdentContextApi {
    def qualifier: Tree = EmptyTree
    def isBackquoted = this.attachments.get[BackquotedIdentifierAttachment.type].isDefined
  }
  object Ident extends IdentExtractor

  case class ReferenceToBoxed(ident: Ident) extends TermTree with ReferenceToBoxedApi {
    override def symbol: Symbol = ident.symbol
    override def symbol_=(sym: Symbol) { ident.symbol = sym }
  }
  object ReferenceToBoxed extends ReferenceToBoxedExtractor

  case class Literal(value: Constant)
        extends TermTree with LiteralApi {
    assert(value ne null)
  }
  object Literal extends LiteralExtractor

//  @deprecated("will be removed and then be re-introduced with changed semantics, use Literal(Constant(x)) instead")
//  def Literal(x: Any) = new Literal(Constant(x))

  case class Annotated(annot: Tree, arg: Tree) extends Tree with AnnotatedApi
  object Annotated extends AnnotatedExtractor

  case class SingletonTypeTree(ref: Tree)
        extends TypTree with SingletonTypeTreeApi
  object SingletonTypeTree extends SingletonTypeTreeExtractor

  case class SelectFromTypeTree(qualifier: Tree, name: TypeName)
       extends TypTree with RefTree with SelectFromTypeTreeApi
  object SelectFromTypeTree extends SelectFromTypeTreeExtractor

  case class CompoundTypeTree(templ: Template)
       extends TypTree with CompoundTypeTreeApi
  object CompoundTypeTree extends CompoundTypeTreeExtractor

  case class AppliedTypeTree(tpt: Tree, args: List[Tree])
       extends TypTree with AppliedTypeTreeApi {
    override def symbol: Symbol = tpt.symbol
    override def symbol_=(sym: Symbol) { tpt.symbol = sym }
  }
  object AppliedTypeTree extends AppliedTypeTreeExtractor

  case class TypeBoundsTree(lo: Tree, hi: Tree)
       extends TypTree with TypeBoundsTreeApi
  object TypeBoundsTree extends TypeBoundsTreeExtractor

  case class ExistentialTypeTree(tpt: Tree, whereClauses: List[Tree])
       extends TypTree with ExistentialTypeTreeApi
  object ExistentialTypeTree extends ExistentialTypeTreeExtractor

  case class TypeTree() extends TypTree with TypeTreeContextApi {
    private var orig: Tree = null
    private[scala] var wasEmpty: Boolean = false

    override def symbol = if (tpe == null) null else tpe.typeSymbol
    override def isEmpty = (tpe eq null) || tpe == NoType

    def original: Tree = orig
    def setOriginal(tree: Tree): this.type = {
      def followOriginal(t: Tree): Tree = t match {
        case tt: TypeTree => followOriginal(tt.original)
        case t => t
      }

      orig = followOriginal(tree); setPos(tree.pos);
      this
    }

    override def defineType(tp: Type): this.type = {
      wasEmpty = isEmpty
      setType(tp)
    }
  }
  object TypeTree extends TypeTreeExtractor

  def TypeTree(tp: Type): TypeTree = TypeTree() setType tp

  class StrictTreeCopier extends TreeCopierOps {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) =
      new ClassDef(mods, name.toTypeName, tparams, impl).copyAttrs(tree)
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]) =
      new PackageDef(pid, stats).copyAttrs(tree)
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) =
      new ModuleDef(mods, name.toTermName, impl).copyAttrs(tree)
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) =
      new ValDef(mods, name.toTermName, tpt, rhs).copyAttrs(tree)
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) =
      new DefDef(mods, name.toTermName, tparams, vparamss, tpt, rhs).copyAttrs(tree)
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) =
      new TypeDef(mods, name.toTypeName, tparams, rhs).copyAttrs(tree)
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) =
      new LabelDef(name.toTermName, params, rhs).copyAttrs(tree)
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]) =
      new Import(expr, selectors).copyAttrs(tree)
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]) =
      new Template(parents, self, body).copyAttrs(tree)
    def Block(tree: Tree, stats: List[Tree], expr: Tree) =
      new Block(stats, expr).copyAttrs(tree)
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) =
      new CaseDef(pat, guard, body).copyAttrs(tree)
    def Alternative(tree: Tree, trees: List[Tree]) =
      new Alternative(trees).copyAttrs(tree)
    def Star(tree: Tree, elem: Tree) =
      new Star(elem).copyAttrs(tree)
    def Bind(tree: Tree, name: Name, body: Tree) =
      new Bind(name, body).copyAttrs(tree)
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]) =
      new UnApply(fun, args).copyAttrs(tree)
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]) =
      new ArrayValue(elemtpt, trees).copyAttrs(tree)
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) =
      new Function(vparams, body).copyAttrs(tree)
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) =
      new Assign(lhs, rhs).copyAttrs(tree)
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree) =
      new AssignOrNamedArg(lhs, rhs).copyAttrs(tree)
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) =
      new If(cond, thenp, elsep).copyAttrs(tree)
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =
      new Match(selector, cases).copyAttrs(tree)
    def Return(tree: Tree, expr: Tree) =
      new Return(expr).copyAttrs(tree)
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) =
      new Try(block, catches, finalizer).copyAttrs(tree)
    def Throw(tree: Tree, expr: Tree) =
      new Throw(expr).copyAttrs(tree)
    def New(tree: Tree, tpt: Tree) =
      new New(tpt).copyAttrs(tree)
    def Typed(tree: Tree, expr: Tree, tpt: Tree) =
      new Typed(expr, tpt).copyAttrs(tree)
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) =
      new TypeApply(fun, args).copyAttrs(tree)
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) =
      (tree match { // TODO: use a tree attachment to track whether this is an apply to implicit args or a view
        case _: ApplyToImplicitArgs => new ApplyToImplicitArgs(fun, args)
        case _: ApplyImplicitView => new ApplyImplicitView(fun, args)
        // TODO: ApplyConstructor ???
        case _ => new Apply(fun, args)
      }).copyAttrs(tree)
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]) =
      new ApplyDynamic(qual, args).copyAttrs(tree)
    def Super(tree: Tree, qual: Tree, mix: TypeName) =
      new Super(qual, mix).copyAttrs(tree)
    def This(tree: Tree, qual: Name) =
      new This(qual.toTypeName).copyAttrs(tree)
    def Select(tree: Tree, qualifier: Tree, selector: Name) =
      new Select(qualifier, selector).copyAttrs(tree)
    def Ident(tree: Tree, name: Name) =
      new Ident(name) copyAttrs tree
    def ReferenceToBoxed(tree: Tree, idt: Ident) =
      new ReferenceToBoxed(idt).copyAttrs(tree)
    def Literal(tree: Tree, value: Constant) =
      new Literal(value).copyAttrs(tree)
    def TypeTree(tree: Tree) =
      new TypeTree().copyAttrs(tree)
    def Annotated(tree: Tree, annot: Tree, arg: Tree) =
      new Annotated(annot, arg).copyAttrs(tree)
    def SingletonTypeTree(tree: Tree, ref: Tree) =
      new SingletonTypeTree(ref).copyAttrs(tree)
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) =
      new SelectFromTypeTree(qualifier, selector.toTypeName).copyAttrs(tree)
    def CompoundTypeTree(tree: Tree, templ: Template) =
      new CompoundTypeTree(templ).copyAttrs(tree)
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) =
      new AppliedTypeTree(tpt, args).copyAttrs(tree)
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree) =
      new TypeBoundsTree(lo, hi).copyAttrs(tree)
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]) =
      new ExistentialTypeTree(tpt, whereClauses).copyAttrs(tree)
  }

  class LazyTreeCopier extends TreeCopierOps {
    val treeCopy: TreeCopier = newStrictTreeCopier
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) = tree match {
      case t @ ClassDef(mods0, name0, tparams0, impl0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) && (impl0 == impl) => t
      case _ => treeCopy.ClassDef(tree, mods, name, tparams, impl)
    }
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]) = tree match {
      case t @ PackageDef(pid0, stats0)
      if (pid0 == pid) && (stats0 == stats) => t
      case _ => treeCopy.PackageDef(tree, pid, stats)
    }
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) = tree match {
      case t @ ModuleDef(mods0, name0, impl0)
      if (mods0 == mods) && (name0 == name) && (impl0 == impl) => t
      case _ => treeCopy.ModuleDef(tree, mods, name, impl)
    }
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) = tree match {
      case t @ ValDef(mods0, name0, tpt0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tpt0 == tpt) && (rhs0 == rhs) => t
      case _ => treeCopy.ValDef(tree, mods, name, tpt, rhs)
    }
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) = tree match {
      case t @ DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) &&
         (vparamss0 == vparamss) && (tpt0 == tpt) && (rhs == rhs0) => t
      case _ => treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs)
    }
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) = tree match {
      case t @ TypeDef(mods0, name0, tparams0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) && (rhs0 == rhs) => t
      case _ => treeCopy.TypeDef(tree, mods, name, tparams, rhs)
    }
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) = tree match {
      case t @ LabelDef(name0, params0, rhs0)
      if (name0 == name) && (params0 == params) && (rhs0 == rhs) => t
      case _ => treeCopy.LabelDef(tree, name, params, rhs)
    }
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]) = tree match {
      case t @ Import(expr0, selectors0)
      if (expr0 == expr) && (selectors0 == selectors) => t
      case _ => treeCopy.Import(tree, expr, selectors)
    }
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]) = tree match {
      case t @ Template(parents0, self0, body0)
      if (parents0 == parents) && (self0 == self) && (body0 == body) => t
      case _ => treeCopy.Template(tree, parents, self, body)
    }
    def Block(tree: Tree, stats: List[Tree], expr: Tree) = tree match {
      case t @ Block(stats0, expr0)
      if ((stats0 == stats) && (expr0 == expr)) => t
      case _ => treeCopy.Block(tree, stats, expr)
    }
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) = tree match {
      case t @ CaseDef(pat0, guard0, body0)
      if (pat0 == pat) && (guard0 == guard) && (body0 == body) => t
      case _ => treeCopy.CaseDef(tree, pat, guard, body)
    }
    def Alternative(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Alternative(trees0)
      if trees0 == trees => t
      case _ => treeCopy.Alternative(tree, trees)
    }
    def Star(tree: Tree, elem: Tree) = tree match {
      case t @ Star(elem0)
      if elem0 == elem => t
      case _ => treeCopy.Star(tree, elem)
    }
    def Bind(tree: Tree, name: Name, body: Tree) = tree match {
      case t @ Bind(name0, body0)
      if (name0 == name) && (body0 == body) => t
      case _ => treeCopy.Bind(tree, name, body)
    }
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ UnApply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => treeCopy.UnApply(tree, fun, args)
    }
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]) = tree match {
      case t @ ArrayValue(elemtpt0, trees0)
      if (elemtpt0 == elemtpt) && (trees0 == trees) => t
      case _ => treeCopy.ArrayValue(tree, elemtpt, trees)
    }
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) = tree match {
      case t @ Function(vparams0, body0)
      if (vparams0 == vparams) && (body0 == body) => t
      case _ => treeCopy.Function(tree, vparams, body)
    }
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ Assign(lhs0, rhs0)
      if (lhs0 == lhs) && (rhs0 == rhs) => t
      case _ => treeCopy.Assign(tree, lhs, rhs)
    }
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ AssignOrNamedArg(lhs0, rhs0)
      if (lhs0 == lhs) && (rhs0 == rhs) => t
      case _ => treeCopy.AssignOrNamedArg(tree, lhs, rhs)
    }
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) = tree match {
      case t @ If(cond0, thenp0, elsep0)
      if (cond0 == cond) && (thenp0 == thenp) && (elsep0 == elsep) => t
      case _ => treeCopy.If(tree, cond, thenp, elsep)
    }
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =  tree match {
      case t @ Match(selector0, cases0)
      if (selector0 == selector) && (cases0 == cases) => t
      case _ => treeCopy.Match(tree, selector, cases)
    }
    def Return(tree: Tree, expr: Tree) = tree match {
      case t @ Return(expr0)
      if expr0 == expr => t
      case _ => treeCopy.Return(tree, expr)
    }
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) = tree match {
      case t @ Try(block0, catches0, finalizer0)
      if (block0 == block) && (catches0 == catches) && (finalizer0 == finalizer) => t
      case _ => treeCopy.Try(tree, block, catches, finalizer)
    }
    def Throw(tree: Tree, expr: Tree) = tree match {
      case t @ Throw(expr0)
      if expr0 == expr => t
      case _ => treeCopy.Throw(tree, expr)
    }
    def New(tree: Tree, tpt: Tree) = tree match {
      case t @ New(tpt0)
      if tpt0 == tpt => t
      case _ => treeCopy.New(tree, tpt)
    }
    def Typed(tree: Tree, expr: Tree, tpt: Tree) = tree match {
      case t @ Typed(expr0, tpt0)
      if (expr0 == expr) && (tpt0 == tpt) => t
      case _ => treeCopy.Typed(tree, expr, tpt)
    }
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ TypeApply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => treeCopy.TypeApply(tree, fun, args)
    }
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ Apply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => treeCopy.Apply(tree, fun, args)
    }
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]) = tree match {
      case t @ ApplyDynamic(qual0, args0)
      if (qual0 == qual) && (args0 == args) => t
      case _ => treeCopy.ApplyDynamic(tree, qual, args)
    }
    def Super(tree: Tree, qual: Tree, mix: TypeName) = tree match {
      case t @ Super(qual0, mix0)
      if (qual0 == qual) && (mix0 == mix) => t
      case _ => treeCopy.Super(tree, qual, mix)
    }
    def This(tree: Tree, qual: Name) = tree match {
      case t @ This(qual0)
      if qual0 == qual => t
      case _ => treeCopy.This(tree, qual)
    }
    def Select(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ Select(qualifier0, selector0)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => treeCopy.Select(tree, qualifier, selector)
    }
    def Ident(tree: Tree, name: Name) = tree match {
      case t @ Ident(name0)
      if name0 == name => t
      case _ => treeCopy.Ident(tree, name)
    }
    def ReferenceToBoxed(tree: Tree, idt: Ident) = tree match {
      case t @ ReferenceToBoxed(idt0)
      if (idt0 == idt) => t
      case _ => this.treeCopy.ReferenceToBoxed(tree, idt)
    }
    def Literal(tree: Tree, value: Constant) = tree match {
      case t @ Literal(value0)
      if value0 == value => t
      case _ => treeCopy.Literal(tree, value)
    }
    def TypeTree(tree: Tree) = tree match {
      case t @ TypeTree() => t
      case _ => treeCopy.TypeTree(tree)
    }
    def Annotated(tree: Tree, annot: Tree, arg: Tree) = tree match {
      case t @ Annotated(annot0, arg0)
      if (annot0==annot) => t
      case _ => treeCopy.Annotated(tree, annot, arg)
    }
    def SingletonTypeTree(tree: Tree, ref: Tree) = tree match {
      case t @ SingletonTypeTree(ref0)
      if ref0 == ref => t
      case _ => treeCopy.SingletonTypeTree(tree, ref)
    }
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ SelectFromTypeTree(qualifier0, selector0)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => treeCopy.SelectFromTypeTree(tree, qualifier, selector)
    }
    def CompoundTypeTree(tree: Tree, templ: Template) = tree match {
      case t @ CompoundTypeTree(templ0)
      if templ0 == templ => t
      case _ => treeCopy.CompoundTypeTree(tree, templ)
    }
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) = tree match {
      case t @ AppliedTypeTree(tpt0, args0)
      if (tpt0 == tpt) && (args0 == args) => t
      case _ => treeCopy.AppliedTypeTree(tree, tpt, args)
    }
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree) = tree match {
      case t @ TypeBoundsTree(lo0, hi0)
      if (lo0 == lo) && (hi0 == hi) => t
      case _ => treeCopy.TypeBoundsTree(tree, lo, hi)
    }
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]) = tree match {
      case t @ ExistentialTypeTree(tpt0, whereClauses0)
      if (tpt0 == tpt) && (whereClauses0 == whereClauses) => t
      case _ => treeCopy.ExistentialTypeTree(tree, tpt, whereClauses)
    }
  }

  // Belongs in TreeInfo but then I can't reach it from Printers.
  def isReferenceToScalaMember(t: Tree, Id: Name) = t match {
    case Ident(Id)                                          => true
    case Select(Ident(nme.scala_), Id)                      => true
    case Select(Select(Ident(nme.ROOTPKG), nme.scala_), Id) => true
    case _                                                  => false
  }
  /** Is the tree Predef, scala.Predef, or _root_.scala.Predef?
   */
  def isReferenceToPredef(t: Tree) = isReferenceToScalaMember(t, nme.Predef)
  def isReferenceToAnyVal(t: Tree) = isReferenceToScalaMember(t, tpnme.AnyVal)

  // --- modifiers implementation ---------------------------------------

  /** @param privateWithin the qualifier for a private (a type name)
   *    or tpnme.EMPTY, if none is given.
   *  @param annotations the annotations for the definition.
   *    '''Note:''' the typechecker drops these annotations,
   *    use the AnnotationInfo's (Symbol.annotations) in later phases.
   */
  case class Modifiers(flags: Long,
                       privateWithin: Name,
                       annotations: List[Tree]) extends ModifiersApi with HasFlags {

    var positions: Map[Long, Position] = Map()

    def setPositions(poss: Map[Long, Position]): this.type = {
      positions = poss; this
    }

    /* Abstract types from HasFlags. */
    type AccessBoundaryType = Name
    type AnnotationType     = Tree

    def hasAnnotationNamed(name: TypeName) = {
      annotations exists {
        case Apply(Select(New(Ident(`name`)), _), _)     => true
        case Apply(Select(New(Select(_, `name`)), _), _) => true
        case _                                           => false
      }
    }

    def hasAccessBoundary = privateWithin != tpnme.EMPTY
    def hasAllFlags(mask: Long): Boolean = (flags & mask) == mask
    def hasFlag(flag: Long) = (flag & flags) != 0L

    def & (flag: Long): Modifiers = {
      val flags1 = flags & flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations) setPositions positions
    }
    def &~ (flag: Long): Modifiers = {
      val flags1 = flags & (~flag)
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations) setPositions positions
    }
    def | (flag: Long): Modifiers = {
      val flags1 = flags | flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations) setPositions positions
    }
    def withAnnotations(annots: List[Tree]) =
      if (annots.isEmpty) this
      else copy(annotations = annotations ::: annots) setPositions positions

    def withPosition(flag: Long, position: Position) =
      copy() setPositions positions + (flag -> position)

    override def mapAnnotations(f: List[Tree] => List[Tree]): Modifiers =
      Modifiers(flags, privateWithin, f(annotations)) setPositions positions

    override def toString = "Modifiers(%s, %s, %s)".format(flagString, annotations mkString ", ", positions)
  }

  object Modifiers extends ModifiersCreator

  implicit val ModifiersTag = ClassTag[Modifiers](classOf[Modifiers])

  // ---- values and creators ---------------------------------------

  /** @param sym       the class symbol
   *  @return          the implementation template
   */
  def ClassDef(sym: Symbol, impl: Template): ClassDef =
    atPos(sym.pos) {
      ClassDef(Modifiers(sym.flags),
               sym.name.toTypeName,
               sym.typeParams map TypeDef,
               impl) setSymbol sym
    }

  /**
   *  @param sym       the class symbol
   *  @param impl      the implementation template
   */
  def ModuleDef(sym: Symbol, impl: Template): ModuleDef =
    atPos(sym.pos) {
      ModuleDef(Modifiers(sym.flags), sym.name.toTermName, impl) setSymbol sym
    }

  def ValDef(sym: Symbol, rhs: Tree): ValDef =
    atPos(sym.pos) {
      ValDef(Modifiers(sym.flags), sym.name.toTermName,
             TypeTree(sym.tpe) setPos sym.pos.focus,
             rhs) setSymbol sym
    }

  def ValDef(sym: Symbol): ValDef = ValDef(sym, EmptyTree)

  object emptyValDef extends ValDef(Modifiers(PRIVATE), nme.WILDCARD, TypeTree(NoType), EmptyTree) {
    override def isEmpty = true
    super.setPos(NoPosition)
    override def setPos(pos: Position) = { assert(false); this }
  }

  def DefDef(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef =
    atPos(sym.pos) {
      assert(sym != NoSymbol)
      DefDef(mods,
             sym.name.toTermName,
             sym.typeParams map TypeDef,
             vparamss,
             TypeTree(sym.tpe.finalResultType) setPos sym.pos.focus,
             rhs) setSymbol sym
    }

  def DefDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef =
    DefDef(sym, Modifiers(sym.flags), vparamss, rhs)

  def DefDef(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef =
    DefDef(sym, mods, mapParamss(sym)(ValDef), rhs)

  def DefDef(sym: Symbol, rhs: Tree): DefDef =
    DefDef(sym, Modifiers(sym.flags), rhs)

  def DefDef(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef =
    DefDef(sym, rhs(sym.info.paramss))

  /** A TypeDef node which defines given `sym` with given tight hand side `rhs`. */
  def TypeDef(sym: Symbol, rhs: Tree): TypeDef =
    atPos(sym.pos) {
      TypeDef(Modifiers(sym.flags), sym.name.toTypeName, sym.typeParams map TypeDef, rhs) setSymbol sym
    }

  /** A TypeDef node which defines abstract type or type parameter for given `sym` */
  def TypeDef(sym: Symbol): TypeDef =
    TypeDef(sym, TypeBoundsTree(TypeTree(sym.info.bounds.lo), TypeTree(sym.info.bounds.hi)))

  def LabelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef =
    atPos(sym.pos) {
      LabelDef(sym.name.toTermName, params map Ident, rhs) setSymbol sym
    }

  /** casedef shorthand */
  def CaseDef(pat: Tree, body: Tree): CaseDef =
    CaseDef(pat, EmptyTree, body)

  def Bind(sym: Symbol, body: Tree): Bind =
    Bind(sym.name, body) setSymbol sym

  def Try(body: Tree, cases: (Tree, Tree)*): Try =
    Try(body, cases.toList map { case (pat, rhs) => CaseDef(pat, EmptyTree, rhs) }, EmptyTree)

  def Throw(tpe: Type, args: Tree*): Throw =
    Throw(New(tpe, args: _*))

  def Apply(sym: Symbol, args: Tree*): Tree =
    Apply(Ident(sym), args.toList)

  /** Factory method for object creation `new tpt(args_1)...(args_n)`
   *  A `New(t, as)` is expanded to: `(new t).<init>(as)`
   */
  def New(tpt: Tree, argss: List[List[Tree]]): Tree = argss match {
    case Nil        => ApplyConstructor(tpt, Nil)
    case xs :: rest => {
      def mkApply(fun: Tree, args: List[Tree]) = Apply(fun, args)
      rest.foldLeft(ApplyConstructor(tpt, xs): Tree)(mkApply)
      // [Eugene++] no longer compiles after I moved the `Apply` case class here
      // rest.foldLeft(ApplyConstructor(tpt, xs): Tree)(Apply)
    }
  }

  /** 0-1 argument list new, based on a type.
   */
  def New(tpe: Type, args: Tree*): Tree =
    ApplyConstructor(TypeTree(tpe), args.toList)

  def New(sym: Symbol, args: Tree*): Tree =
    New(sym.tpe, args: _*)

  def Super(sym: Symbol, mix: TypeName): Tree =
    Super(This(sym), mix)

  def This(sym: Symbol): Tree =
    This(sym.name.toTypeName) setSymbol sym

  def Select(qualifier: Tree, name: String): Select =
    Select(qualifier, newTermName(name))

  def Select(qualifier: Tree, sym: Symbol): Select =
    Select(qualifier, sym.name) setSymbol sym

  def Ident(name: String): Ident =
    Ident(newTermName(name))

  def Ident(sym: Symbol): Ident =
    Ident(sym.name) setSymbol sym

  /** Block factory that flattens directly nested blocks.
   */
  def Block(stats: Tree*): Block = {
    if (stats.isEmpty) Block(Nil, Literal(Constant(())))
    else stats match {
      case Seq(b @ Block(_, _)) => b
      case Seq(stat) => Block(stats.toList, Literal(Constant(())))
      case Seq(_, rest @ _*) => Block(stats.init.toList, stats.last)
    }
  }

  // --- generic traversers and transformers

  override protected def itraverse(traverser: Traverser, tree: Tree): Unit = {
    import traverser._
    tree match {
      case EmptyTree =>
        ;
      case PackageDef(pid, stats) =>
        traverse(pid)
        atOwner(mclass(tree.symbol)) {
          traverseTrees(stats)
        }
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(impl)
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(mclass(tree.symbol)) {
          traverseTrees(mods.annotations); traverse(impl)
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverse(tpt); traverse(rhs)
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverseTreess(vparamss); traverse(tpt); traverse(rhs)
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(rhs)
        }
      case LabelDef(name, params, rhs) =>
        traverseTrees(params); traverse(rhs)
      case Import(expr, selectors) =>
        traverse(expr)
      case Annotated(annot, arg) =>
        traverse(annot); traverse(arg)
      case Template(parents, self, body) =>
        traverseTrees(parents)
        if (!self.isEmpty) traverse(self)
        traverseStats(body, tree.symbol)
      case Block(stats, expr) =>
        traverseTrees(stats); traverse(expr)
      case CaseDef(pat, guard, body) =>
        traverse(pat); traverse(guard); traverse(body)
      case Alternative(trees) =>
        traverseTrees(trees)
      case Star(elem) =>
        traverse(elem)
      case Bind(name, body) =>
        traverse(body)
      case UnApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ArrayValue(elemtpt, trees) =>
        traverse(elemtpt); traverseTrees(trees)
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          traverseTrees(vparams); traverse(body)
        }
      case Assign(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case AssignOrNamedArg(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case If(cond, thenp, elsep) =>
        traverse(cond); traverse(thenp); traverse(elsep)
      case Match(selector, cases) =>
        traverse(selector); traverseTrees(cases)
      case Return(expr) =>
        traverse(expr)
      case Try(block, catches, finalizer) =>
        traverse(block); traverseTrees(catches); traverse(finalizer)
      case Throw(expr) =>
        traverse(expr)
      case New(tpt) =>
        traverse(tpt)
      case Typed(expr, tpt) =>
        traverse(expr); traverse(tpt)
      case TypeApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case Apply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ApplyDynamic(qual, args) =>
        traverse(qual); traverseTrees(args)
      case Super(qual, _) =>
        traverse(qual)
      case This(_) =>
        ;
      case Select(qualifier, selector) =>
        traverse(qualifier)
      case Ident(_) =>
        ;
      case ReferenceToBoxed(idt) =>
        traverse(idt)
      case Literal(_) =>
        ;
      case TypeTree() =>
        ;
      case SingletonTypeTree(ref) =>
        traverse(ref)
      case SelectFromTypeTree(qualifier, selector) =>
        traverse(qualifier)
      case CompoundTypeTree(templ) =>
        traverse(templ)
      case AppliedTypeTree(tpt, args) =>
        traverse(tpt); traverseTrees(args)
      case TypeBoundsTree(lo, hi) =>
        traverse(lo); traverse(hi)
      case ExistentialTypeTree(tpt, whereClauses) =>
        traverse(tpt); traverseTrees(whereClauses)
      case _ => xtraverse(traverser, tree)
    }
  }

  override protected def itransform(transformer: Transformer, tree: Tree): Tree = {
    import transformer._
    val treeCopy = transformer.treeCopy
    tree match {
      case EmptyTree =>
        tree
      case PackageDef(pid, stats) =>
        treeCopy.PackageDef(
          tree, transform(pid).asInstanceOf[RefTree],
          atOwner(mclass(tree.symbol)) {
            transformStats(stats, currentOwner)
          }
        )
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          treeCopy.ClassDef(tree, transformModifiers(mods), name,
                            transformTypeDefs(tparams), transformTemplate(impl))
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(mclass(tree.symbol)) {
          treeCopy.ModuleDef(tree, transformModifiers(mods),
                             name, transformTemplate(impl))
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.ValDef(tree, transformModifiers(mods),
                          name, transform(tpt), transform(rhs))
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.DefDef(tree, transformModifiers(mods), name,
                          transformTypeDefs(tparams), transformValDefss(vparamss),
                          transform(tpt), transform(rhs))
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.TypeDef(tree, transformModifiers(mods), name,
                           transformTypeDefs(tparams), transform(rhs))
        }
      case LabelDef(name, params, rhs) =>
        treeCopy.LabelDef(tree, name, transformIdents(params), transform(rhs)) //bq: Martin, once, atOwner(...) works, also change `LamdaLifter.proxy'
      case Import(expr, selectors) =>
        treeCopy.Import(tree, transform(expr), selectors)
      case Template(parents, self, body) =>
        treeCopy.Template(tree, transformTrees(parents), transformValDef(self), transformStats(body, tree.symbol))
      case Block(stats, expr) =>
        treeCopy.Block(tree, transformStats(stats, currentOwner), transform(expr))
      case CaseDef(pat, guard, body) =>
        treeCopy.CaseDef(tree, transform(pat), transform(guard), transform(body))
      case Alternative(trees) =>
        treeCopy.Alternative(tree, transformTrees(trees))
      case Star(elem) =>
        treeCopy.Star(tree, transform(elem))
      case Bind(name, body) =>
        treeCopy.Bind(tree, name, transform(body))
      case UnApply(fun, args) =>
        treeCopy.UnApply(tree, fun, transformTrees(args)) // bq: see test/.../unapplyContexts2.scala
      case ArrayValue(elemtpt, trees) =>
        treeCopy.ArrayValue(tree, transform(elemtpt), transformTrees(trees))
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          treeCopy.Function(tree, transformValDefs(vparams), transform(body))
        }
      case Assign(lhs, rhs) =>
        treeCopy.Assign(tree, transform(lhs), transform(rhs))
      case AssignOrNamedArg(lhs, rhs) =>
        treeCopy.AssignOrNamedArg(tree, transform(lhs), transform(rhs))
      case If(cond, thenp, elsep) =>
        treeCopy.If(tree, transform(cond), transform(thenp), transform(elsep))
      case Match(selector, cases) =>
        treeCopy.Match(tree, transform(selector), transformCaseDefs(cases))
      case Return(expr) =>
        treeCopy.Return(tree, transform(expr))
      case Try(block, catches, finalizer) =>
        treeCopy.Try(tree, transform(block), transformCaseDefs(catches), transform(finalizer))
      case Throw(expr) =>
        treeCopy.Throw(tree, transform(expr))
      case New(tpt) =>
        treeCopy.New(tree, transform(tpt))
      case Typed(expr, tpt) =>
        treeCopy.Typed(tree, transform(expr), transform(tpt))
      case TypeApply(fun, args) =>
        treeCopy.TypeApply(tree, transform(fun), transformTrees(args))
      case Apply(fun, args) =>
        treeCopy.Apply(tree, transform(fun), transformTrees(args))
      case ApplyDynamic(qual, args) =>
        treeCopy.ApplyDynamic(tree, transform(qual), transformTrees(args))
      case Super(qual, mix) =>
        treeCopy.Super(tree, transform(qual), mix)
      case This(qual) =>
        treeCopy.This(tree, qual)
      case Select(qualifier, selector) =>
        treeCopy.Select(tree, transform(qualifier), selector)
      case Ident(name) =>
        treeCopy.Ident(tree, name)
      case ReferenceToBoxed(idt) =>
        treeCopy.ReferenceToBoxed(tree, transform(idt) match { case idt1: Ident => idt1 })
      case Literal(value) =>
        treeCopy.Literal(tree, value)
      case TypeTree() =>
        treeCopy.TypeTree(tree)
      case Annotated(annot, arg) =>
        treeCopy.Annotated(tree, transform(annot), transform(arg))
      case SingletonTypeTree(ref) =>
        treeCopy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        treeCopy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case CompoundTypeTree(templ) =>
        treeCopy.CompoundTypeTree(tree, transformTemplate(templ))
      case AppliedTypeTree(tpt, args) =>
        treeCopy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
      case TypeBoundsTree(lo, hi) =>
        treeCopy.TypeBoundsTree(tree, transform(lo), transform(hi))
      case ExistentialTypeTree(tpt, whereClauses) =>
        treeCopy.ExistentialTypeTree(tree, transform(tpt), transformTrees(whereClauses))
      case _ =>
        xtransform(transformer, tree)
    }
  }

  private def mclass(sym: Symbol) = sym map (_.asModuleSymbol.moduleClass)

  // --- specific traversers and transformers

  @deprecated("Moved to tree.duplicate", "2.10.0")
  protected[scala] def duplicateTree(tree: Tree): Tree = tree.duplicate

  class ForeachPartialTreeTraverser(pf: PartialFunction[Tree, Tree]) extends Traverser {
    override def traverse(tree: Tree) {
      val t = if (pf isDefinedAt tree) pf(tree) else tree
      super.traverse(t)
    }
  }

  class ChangeOwnerTraverser(val oldowner: Symbol, val newowner: Symbol) extends Traverser {
    def changeOwner(tree: Tree) = tree match {
      case Return(expr) =>
        if (tree.symbol == oldowner) {
          // SI-5612
          if (newowner hasTransOwner oldowner)
            log("NOT changing owner of %s because %s is nested in %s".format(tree, newowner, oldowner))
          else {
            log("changing owner of %s: %s => %s".format(tree, oldowner, newowner))
            tree.symbol = newowner
          }
        }
      case _: DefTree | _: Function =>
        if (tree.symbol != NoSymbol && tree.symbol.owner == oldowner) {
          tree.symbol.owner = newowner
        }
      case _ =>
    }
    override def traverse(tree: Tree) {
      changeOwner(tree)
      super.traverse(tree)
    }
  }

  private class ShallowDuplicator(orig: Tree) extends Transformer {
    override val treeCopy = newStrictTreeCopier
    override def transform(tree: Tree) =
      if (tree eq orig) super.transform(tree)
      else tree
  }
  // Create a readable string describing a substitution.
  private def substituterString(fromStr: String, toStr: String, from: List[Any], to: List[Any]): String = {
    "subst[%s, %s](%s)".format(fromStr, toStr, (from, to).zipped map (_ + " -> " + _) mkString ", ")
  }

  // NOTE: calls shallowDuplicate on trees in `to` to avoid problems when symbols in `from`
  // occur multiple times in the `tree` passed to `transform`,
  // otherwise, the resulting Tree would be a graph, not a tree... this breaks all sorts of stuff,
  // notably concerning the mutable aspects of Trees (such as setting their .tpe)
  class TreeSubstituter(from: List[Symbol], to: List[Tree]) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(_) =>
        def subst(from: List[Symbol], to: List[Tree]): Tree =
          if (from.isEmpty) tree
          else if (tree.symbol == from.head) to.head.shallowDuplicate // TODO: does it ever make sense *not* to perform a shallowDuplicate on `to.head`?
          else subst(from.tail, to.tail);
        subst(from, to)
      case _ =>
        super.transform(tree)
    }
    override def toString = substituterString("Symbol", "Tree", from, to)
  }

  /** Substitute clazz.this with `to`. `to` must be an attributed tree.
   */
  class ThisSubstituter(clazz: Symbol, to: => Tree) extends Transformer {
    val newtpe = to.tpe
    override def transform(tree: Tree) = {
      if (tree.tpe ne null) tree.tpe = tree.tpe.substThis(clazz, newtpe)
      tree match {
        case This(_) if tree.symbol == clazz => to
        case _ => super.transform(tree)
      }
    }
  }

  class TypeMapTreeSubstituter(val typeMap: TypeMap) extends Traverser {
    override def traverse(tree: Tree) {
      if (tree.tpe ne null)
        tree.tpe = typeMap(tree.tpe)
      if (tree.isDef)
        tree.symbol modifyInfo typeMap

      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
  }

  class TreeTypeSubstituter(val from: List[Symbol], val to: List[Type]) extends TypeMapTreeSubstituter(new SubstTypeMap(from, to)) {
    def isEmpty = from.isEmpty && to.isEmpty
    override def toString() = "TreeTypeSubstituter("+from+","+to+")"
  }

  lazy val EmptyTreeTypeSubstituter = new TreeTypeSubstituter(List(), List())

  class TreeSymSubstTraverser(val from: List[Symbol], val to: List[Symbol]) extends TypeMapTreeSubstituter(new SubstSymMap(from, to)) {
    override def toString() = "TreeSymSubstTraverser/" + substituterString("Symbol", "Symbol", from, to)
  }

  /** Substitute symbols in `from` with symbols in `to`. Returns a new
   *  tree using the new symbols and whose Ident and Select nodes are
   *  name-consistent with the new symbols.
   */
  class TreeSymSubstituter(from: List[Symbol], to: List[Symbol]) extends Transformer {
    val symSubst = new SubstSymMap(from, to)
    override def transform(tree: Tree): Tree = {
      def subst(from: List[Symbol], to: List[Symbol]) {
        if (!from.isEmpty)
          if (tree.symbol == from.head) tree setSymbol to.head
          else subst(from.tail, to.tail)
      }

      if (tree.tpe ne null) tree.tpe = symSubst(tree.tpe)
      if (tree.hasSymbol) {
        subst(from, to)
        tree match {
          case Ident(name0) if tree.symbol != NoSymbol =>
            treeCopy.Ident(tree, tree.symbol.name)
          case Select(qual, name0) if tree.symbol != NoSymbol =>
            treeCopy.Select(tree, transform(qual), tree.symbol.name)
          case _ =>
            super.transform(tree)
        }
      } else
        super.transform(tree)
    }
    def apply[T <: Tree](tree: T): T = transform(tree).asInstanceOf[T]
    override def toString() = "TreeSymSubstituter/" + substituterString("Symbol", "Symbol", from, to)
  }


  class ForeachTreeTraverser(f: Tree => Unit) extends Traverser {
    override def traverse(t: Tree) {
      f(t)
      super.traverse(t)
    }
  }

  class FilterTreeTraverser(p: Tree => Boolean) extends Traverser {
    val hits = new ListBuffer[Tree]
    override def traverse(t: Tree) {
      if (p(t)) hits += t
      super.traverse(t)
    }
  }

  class CollectTreeTraverser[T](pf: PartialFunction[Tree, T]) extends Traverser {
    val results = new ListBuffer[T]
    override def traverse(t: Tree) {
      if (pf.isDefinedAt(t)) results += pf(t)
      super.traverse(t)
    }
  }

  class FindTreeTraverser(p: Tree => Boolean) extends Traverser {
    var result: Option[Tree] = None
    override def traverse(t: Tree) {
      if (result.isEmpty) {
        if (p(t)) result = Some(t)
        super.traverse(t)
      }
    }
  }

  private lazy val duplicator = new Transformer {
    override val treeCopy = newStrictTreeCopier
    override def transform(t: Tree) = {
      val t1 = super.transform(t)
      if ((t1 ne t) && t1.pos.isRange) t1 setPos t.pos.focus
      t1
    }
  }

  // ------ copiers -------------------------------------------

  def copyDefDef(tree: Tree)(
    mods: Modifiers              = null,
    name: Name                   = null,
    tparams: List[TypeDef]       = null,
    vparamss: List[List[ValDef]] = null,
    tpt: Tree                    = null,
    rhs: Tree                    = null
  ): DefDef = tree match {
    case DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0) =>
      treeCopy.DefDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (tparams eq null) tparams0 else tparams,
        if (vparamss eq null) vparamss0 else vparamss,
        if (tpt eq null) tpt0 else tpt,
        if (rhs eq null) rhs0 else rhs
      )
    case t =>
      sys.error("Not a DefDef: " + t + "/" + t.getClass)
  }
  def copyValDef(tree: Tree)(
    mods: Modifiers = null,
    name: Name      = null,
    tpt: Tree       = null,
    rhs: Tree       = null
  ): ValDef = tree match {
    case ValDef(mods0, name0, tpt0, rhs0) =>
      treeCopy.ValDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (tpt eq null) tpt0 else tpt,
        if (rhs eq null) rhs0 else rhs
      )
    case t =>
      sys.error("Not a ValDef: " + t + "/" + t.getClass)
  }
  def copyClassDef(tree: Tree)(
    mods: Modifiers        = null,
    name: Name             = null,
    tparams: List[TypeDef] = null,
    impl: Template         = null
  ): ClassDef = tree match {
    case ClassDef(mods0, name0, tparams0, impl0) =>
      treeCopy.ClassDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (tparams eq null) tparams0 else tparams,
        if (impl eq null) impl0 else impl
      )
    case t =>
      sys.error("Not a ClassDef: " + t + "/" + t.getClass)
  }

  def deriveDefDef(ddef: Tree)(applyToRhs: Tree => Tree): DefDef = ddef match {
    case DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0) =>
      treeCopy.DefDef(ddef, mods0, name0, tparams0, vparamss0, tpt0, applyToRhs(rhs0))
    case t =>
      sys.error("Not a DefDef: " + t + "/" + t.getClass)
  }
  def deriveValDef(vdef: Tree)(applyToRhs: Tree => Tree): ValDef = vdef match {
    case ValDef(mods0, name0, tpt0, rhs0) =>
      treeCopy.ValDef(vdef, mods0, name0, tpt0, applyToRhs(rhs0))
    case t =>
      sys.error("Not a ValDef: " + t + "/" + t.getClass)
  }
  def deriveTemplate(templ: Tree)(applyToBody: List[Tree] => List[Tree]): Template = templ match {
    case Template(parents0, self0, body0) =>
      treeCopy.Template(templ, parents0, self0, applyToBody(body0))
    case t =>
      sys.error("Not a Template: " + t + "/" + t.getClass)
  }
  def deriveClassDef(cdef: Tree)(applyToImpl: Template => Template): ClassDef = cdef match {
    case ClassDef(mods0, name0, tparams0, impl0) =>
      treeCopy.ClassDef(cdef, mods0, name0, tparams0, applyToImpl(impl0))
    case t =>
      sys.error("Not a ClassDef: " + t + "/" + t.getClass)
  }
  def deriveModuleDef(mdef: Tree)(applyToImpl: Template => Template): ModuleDef = mdef match {
    case ModuleDef(mods0, name0, impl0) =>
      treeCopy.ModuleDef(mdef, mods0, name0, applyToImpl(impl0))
    case t =>
      sys.error("Not a ModuleDef: " + t + "/" + t.getClass)
  }
  def deriveCaseDef(cdef: Tree)(applyToBody: Tree => Tree): CaseDef = cdef match {
    case CaseDef(pat0, guard0, body0) =>
      treeCopy.CaseDef(cdef, pat0, guard0, applyToBody(body0))
    case t =>
      sys.error("Not a CaseDef: " + t + "/" + t.getClass)
  }
  def deriveLabelDef(ldef: Tree)(applyToRhs: Tree => Tree): LabelDef = ldef match {
    case LabelDef(name0, params0, rhs0) =>
      treeCopy.LabelDef(ldef, name0, params0, applyToRhs(rhs0))
    case t =>
      sys.error("Not a LabelDef: " + t + "/" + t.getClass)
  }

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

  val treeNodeCount = Statistics.newView("#created tree nodes")(nodeCount)
}

object TreesStats {
  // statistics
  val nodeByType = Statistics.newByClass("#created tree nodes by type")(Statistics.newCounter(""))
}
