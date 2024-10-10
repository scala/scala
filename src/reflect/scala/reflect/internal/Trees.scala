/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package internal

import Flags._
import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.Attachments
import util.{ReusableInstance, Statistics, StringContextStripMarginOps}

trait Trees extends api.Trees {
  self: SymbolTable =>

  private[scala] var nodeCount = 0

  protected def treeLine(t: Tree): String =
    if (t.pos.isDefined && t.pos.isRange) t.pos.lineContent.drop(t.pos.column - 1).take(t.pos.end - t.pos.start + 1)
    else t.summaryString

  protected def treeStatus(t: Tree, enclosingTree: Tree = null) = {
    val parent = if (enclosingTree eq null) "        " else " P#%5s".format(enclosingTree.id)

    "[L%4s%8s] #%-6s %-15s %-10s // %s".format(t.pos.line, parent, t.id, t.pos.show, t.shortClass, treeLine(t))
  }
  protected def treeSymStatus(t: Tree) = {
    val line = if (t.pos.isDefined) "line %-4s".format(t.pos.line) else "         "
    "#%-5s %s %-10s // %s".format(t.id, line, t.shortClass,
      if (t.symbol ne NoSymbol) "(" + t.symbol.fullLocationString + ")"
      else treeLine(t)
    )
  }

  abstract class Tree extends TreeContextApiImpl with Attachable with Product {
    val id = nodeCount // TODO: add to attachment?
    nodeCount += 1

    final override def pos: Position = rawatt.pos

    private[this] var rawtpe: Type = _
    final def tpe = rawtpe
    @deprecated("use setType", "2.11.0") def tpe_=(t: Type): Unit = setType(t)

    def clearType(): this.type = this setType null
    def setType(tp: Type): this.type = { rawtpe = tp; this }
    def defineType(tp: Type): this.type = setType(tp)

    def symbol: Symbol = null //!!!OPT!!! symbol is about 3% of hot compile times -- megamorphic dispatch?
    def symbol_=(sym: Symbol): Unit = { throw new UnsupportedOperationException("symbol_= inapplicable for " + this) }
    def setSymbol(sym: Symbol): this.type = { symbol = sym; this }
    def hasSymbolField = false
    @deprecated("use hasSymbolField", "2.11.0") def hasSymbol = hasSymbolField

    def isDef = false

    def isEmpty = false
    def nonEmpty = !isEmpty

    def canHaveAttrs = true

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
      setType(tree.tpe)
      if (hasSymbolField) symbol = tree.symbol
      this
    }

    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]

    override def duplicate: this.type =
      (duplicator transform this).asInstanceOf[this.type]
  }

  abstract class TreeContextApiImpl extends TreeApi { this: Tree =>

    override def orElse(alt: => Tree) = if (!isEmpty) this else alt

    override def foreach(f: Tree => Unit): Unit = { new ForeachTreeTraverser(f).traverse(this) }

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
      var builder: ListBuffer[Tree] = null
      def subtrees(x: Any): Unit = x match {
        case EmptyTree =>
        case t: Tree =>
          if (builder eq null) builder = new ListBuffer[Tree]
          builder += t
        case xs: List[_] => xs foreach subtrees
        case _ =>
      }
      productIterator foreach subtrees
      if (builder eq null) Nil else builder.result()
    }
    /** Equivalent to `{this.children.takeWhile(f); ()}, but more efficient` */
    final def foreachChild(f: Tree => Boolean): Unit = {
      def subtrees(x: Any): Boolean = x match {
        case EmptyTree =>
          true
        case t: Tree =>
          f(t)
        case xs: List[_] =>
          var rest = xs
          while (!rest.isEmpty) {
            if (!subtrees(rest.head)) return false
            rest = rest.tail
          }
          true
        case _ =>
          true
      }
      val N = productArity
      var i = 0
      while (i < N) {
        if (!subtrees(productElement(i))) return
        i += 1
      }
    }

    /** Equivalent to `this.children.headOption.getOrElse(EmptyTree)`, but more efficient. */
    final def onlyChild: Tree = {
      onlyChildAccumulator.using(accum => { foreachChild(accum); accum.result()})
    }

    def freeTerms: List[FreeTermSymbol] = freeSyms(terms = true, types = false).asInstanceOf[List[FreeTermSymbol]]
    def freeTypes: List[FreeTypeSymbol] = freeSyms(terms = false, types = true).asInstanceOf[List[FreeTypeSymbol]]
    def freeSyms: List[FreeSymbol] = freeSyms(terms = true, types = true)

    private def freeSyms(terms: Boolean, types: Boolean): List[FreeSymbol] = {
      val s = mutable.LinkedHashSet[FreeSymbol]()
      def addIfFree(sym: Symbol): Unit = if (sym != null && (terms && sym.isFreeTerm || types && sym.isFreeType)) s += sym.asInstanceOf[FreeSymbol]
      for (t <- this) {
        addIfFree(t.symbol)
        if (t.tpe != null) {
          for (tp <- t.tpe) {
            if (types) addIfFree(tp.typeSymbol)
            if (types) addIfFree(tp.termSymbol)
          }
        }
      }
      s.toList
    }

    def substituteSymbols(from: List[Symbol], to: List[Symbol]): Tree =
      new TreeSymSubstituter(from, to)(this)

    def substituteTypes(from: List[Symbol], to: List[Type]): Tree =
      new TreeTypeSubstituter(from, to)(this)

    def substituteThis(clazz: Symbol, to: => Tree): Tree =
      new ThisSubstituter(clazz, to) transform this

    def hasExistingSymbol = (symbol ne null) && (symbol ne NoSymbol)
    def hasSymbolWhich(f: Symbol => Boolean) = hasExistingSymbol && f(symbol)

    def isErroneous = (tpe ne null) && tpe.isErroneous
    def isTyped     = (tpe ne null) && !tpe.isErroneous

    /** Sets the tree's type to the result of the given function.
     *  If the type is null, it remains null - the function is not called.
     */
    def modifyType(f: Type => Type): this.type =
      if (tpe eq null) this
      else this setType f(tpe)

    /** If `pf` is defined for a given subtree, call super.traverse(pf(tree)),
     *  otherwise super.traverse(tree).
     */
    def foreachPartial(pf: PartialFunction[Tree, Tree]): Unit = {
      new ForeachPartialTreeTraverser(pf).traverse(this)
    }

    def changeOwner(pairs: (Symbol, Symbol)*): this.type = {
      pairs.foreach {
        case (oldOwner, newOwner) => changeOwner(oldOwner, newOwner)
      }
      this
    }

    def changeOwner(from: Symbol, to: Symbol): this.type =
      new ChangeOwnerTraverser(from, to).apply(this)

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
    def transform(transformer: Transformer): Tree = xtransform(transformer, this)
    def traverse(traverser: Traverser): Unit = xtraverse(traverser, this): @nowarn("cat=deprecation")
  }

  trait TermTree extends Tree with TermTreeApi

  trait TypTree extends Tree with TypTreeApi

  abstract class SymTree extends Tree with SymTreeApi {
    override def hasSymbolField = true
    override var symbol: Symbol = NoSymbol
  }

  trait NameTree extends Tree with NameTreeApi {
    def name: Name
    def getterName: TermName = name.getterName
    def setterName: TermName = name.setterName
    def localName: TermName = name.localName
    def namePos: Position = this.attachments.get[NamePos].map(_.pos).getOrElse(this.pos)
  }

  trait RefTree extends SymTree with NameTree with RefTreeApi {
    def qualifier: Tree    // empty for Idents
    def name: Name
  }

  object RefTree extends RefTreeExtractor {
    def apply(qualifier: Tree, name: Name): RefTree = qualifier match {
      case EmptyTree =>
        Ident(name)
      case qual if qual.isTerm =>
        Select(qual, name)
      case qual if qual.isType =>
        assert(name.isTypeName, s"qual = $qual, name = $name")
        SelectFromTypeTree(qual, name.toTypeName)
      case x => throw new MatchError(x)
    }
    def unapply(refTree: RefTree): Option[(Tree, Name)] = Some((refTree.qualifier, refTree.name))
  }

  sealed abstract class DefTree extends SymTree with NameTree with DefTreeApi {
    def name: Name
    override def isDef = true
  }

  sealed abstract class MemberDef extends DefTree with MemberDefApi {
    def mods: Modifiers

    def keyword: String = this match {
      case TypeDef(_, _, _, _)      => "type"
      case ClassDef(mods, _, _, _)  => if (mods hasFlag TRAIT) "trait" else "class"
      case DefDef(_, _, _, _, _, _) => "def"
      case ModuleDef(_, _, _)       => "object"
      case PackageDef(_, _)         => "package"
      case ValDef(mods, _, _, _)    => if (mods hasFlag MUTABLE) "var" else "val"
    }
  }

  case class PackageDef(pid: RefTree, stats: List[Tree])
       extends MemberDef with PackageDefApi {
    def name = pid.name
    def mods = NoMods
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.PackageDef(
        this, transformer.transform(pid).asInstanceOf[RefTree],
        transformer.atOwner(mclass(this.symbol)) {
          transformer.transformStats(stats, transformer.currentOwner)
        }
      )
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(pid)
      traverser.traverseStats(stats, mclass(this.symbol))
    }
  }
  object PackageDef extends PackageDefExtractor

  sealed abstract class ImplDef extends MemberDef with ImplDefApi {
    def impl: Template
  }

  case class ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template)
       extends ImplDef with ClassDefApi {
    override def transform(transformer: Transformer): Tree =
      transformer.atOwner(this.symbol) {
        transformer.treeCopy.ClassDef(this, transformer.transformModifiers(mods), name,
          transformer.transformTypeDefs(tparams), transformer.transformTemplate(impl))
      }
    override def traverse(traverser: Traverser): Unit = traverser.atOwner(symbol) {
      traverser.traverseModifiers(mods)
      traverser.traverseName(name)
      traverser.traverseParams(tparams)
      traverser.traverse(impl)
    }
  }
  object ClassDef extends ClassDefExtractor {
    /** @param sym       the class symbol
     *  @param impl      the implementation template
     *  @return          the class definition
     */
    def apply(sym: Symbol, impl: Template): ClassDef =
      atPos(sym.pos) {
        ClassDef(Modifiers(sym.flags),
                 sym.name.toTypeName,
                 sym.typeParams map TypeDef.apply,
                 impl) setSymbol sym
      }

    /** @param sym       the class symbol
     *  @param body      trees that constitute the body of the class
     *  @return          the class definition
     */
    def apply(sym: Symbol, body: List[Tree]): ClassDef =
      ClassDef(sym, Template(sym, body))
  }

  case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)
        extends ImplDef with ModuleDefApi {
    override def transform(transformer: Transformer): Tree =
      transformer.atOwner(mclass(this.symbol)) {
        transformer.treeCopy.ModuleDef(this, transformer.transformModifiers(mods),
          name, transformer.transformTemplate(impl))
      }
    override def traverse(traverser: Traverser): Unit = traverser.atOwner(mclass(symbol)) {
      traverser.traverseModifiers(mods)
      traverser.traverseName(name)
      traverser.traverse(impl)
    }
  }
  object ModuleDef extends ModuleDefExtractor {
    /**
     *  @param sym       the class symbol
     *  @param impl      the implementation template
     */
    def apply(sym: Symbol, impl: Template): ModuleDef =
      atPos(sym.pos) {
        ModuleDef(Modifiers(sym.flags), sym.name.toTermName, impl) setSymbol sym
      }
  }

  sealed abstract class ValOrDefDef extends MemberDef with ValOrDefDefApi {
    def name: TermName
    def tpt: Tree
    def rhs: Tree
  }

  object ValOrDefDef {
    def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)] = tree match {
      case ValDef(mods, name, tpt, rhs)       => Some((mods, name, tpt, rhs))
      case DefDef(mods, name, _, _, tpt, rhs) => Some((mods, name, tpt, rhs))
      case _                                  => None
    }
  }

  case class ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree) extends ValOrDefDef with ValDefApi {
    override def transform(transformer: Transformer): Tree =
      transformer.atOwner(this.symbol) {
        transformer.treeCopy.ValDef(this, transformer.transformModifiers(mods),
          name, transformer.transform(tpt), transformer.transform(rhs))
      }
    override def traverse(traverser: Traverser): Unit = traverser.atOwner(symbol) {
      traverser.traverseModifiers(mods)
      traverser.traverseName(name)
      traverser.traverseTypeAscription(tpt)
      traverser.traverse(rhs)
    }
  }
  object ValDef extends ValDefExtractor {
    def apply(sym: Symbol): ValDef            = newValDef(sym, EmptyTree)()
    def apply(sym: Symbol, rhs: Tree): ValDef = newValDef(sym, rhs)()
  }

  case class DefDef(mods: Modifiers, name: TermName, tparams: List[TypeDef],
                    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) extends ValOrDefDef with DefDefApi {
    override def transform(transformer: Transformer): Tree =
      transformer.atOwner(this.symbol) {
        transformer.treeCopy.DefDef(this, transformer.transformModifiers(mods), name,
          transformer.transformTypeDefs(tparams), transformer.transformValDefss(vparamss),
          transformer.transform(tpt), transformer.transform(rhs))
      }
    override def traverse(traverser: Traverser): Unit = traverser.atOwner(symbol) {
      traverser.traverseModifiers(mods)
      traverser.traverseName(name)
      traverser.traverseParams(tparams)
      traverser.traverseParamss(vparamss)
      traverser.traverseTypeAscription(tpt)
      traverser.traverse(rhs)
    }

  }
  object DefDef extends DefDefExtractor {
    def apply(sym: Symbol, rhs: Tree): DefDef                                                = newDefDef(sym, rhs)()
    def apply(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef                  = newDefDef(sym, rhs)(vparamss = vparamss)
    def apply(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef                               = newDefDef(sym, rhs)(mods = mods)
    def apply(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef = newDefDef(sym, rhs)(mods = mods, vparamss = vparamss)
    def apply(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef                          = newDefDef(sym, rhs(sym.info.paramss))()
  }

  case class TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree)
       extends MemberDef with TypeDefApi {
    override def transform(transformer: Transformer): Tree =
      transformer.atOwner(this.symbol) {
        transformer.treeCopy.TypeDef(this, transformer.transformModifiers(mods), name,
          transformer.transformTypeDefs(tparams), transformer.transform(rhs))
      }
    override def traverse(traverser: Traverser): Unit = traverser.atOwner(symbol) {
      traverser.traverseModifiers(mods)
      traverser.traverseName(name)
      traverser.traverseParams(tparams)
      traverser.traverse(rhs)
    }
  }
  object TypeDef extends TypeDefExtractor {
    /** A TypeDef node which defines abstract type or type parameter for given `sym` */
    def apply(sym: Symbol): TypeDef            = newTypeDef(sym, TypeBoundsTree(sym))()
    def apply(sym: Symbol, rhs: Tree): TypeDef = newTypeDef(sym, rhs)()
  }

  case class LabelDef(name: TermName, params: List[Ident], rhs: Tree)
       extends DefTree with TermTree with LabelDefApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.LabelDef(this, name, transformer.transformIdents(params), transformer.transform(rhs)) //bq: Martin, once, atOwner(...) works, also change `LambdaLifter.proxy`
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverseName(name)
      traverser.traverseParams(params)
      traverser.traverse(rhs)
    }
  }
  object LabelDef extends LabelDefExtractor {
    def apply(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef =
      atPos(sym.pos) {
        LabelDef(sym.name.toTermName, params map Ident, rhs) setSymbol sym
      }
  }

  case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int) extends ImportSelectorApi {
    assert(isWildcard || rename != null, s"Bad import selector $name => $rename")
    def isWildcard = name == nme.WILDCARD && rename == null
    def isGiven    = name == nme.WILDCARD && rename == nme.`given`
    def isMask     = name != nme.WILDCARD && rename == nme.WILDCARD
    def isRename   = name != rename && rename != null && rename != nme.WILDCARD
    def isSpecific = !isWildcard
    private def isLiteralWildcard = name == nme.WILDCARD && rename == nme.WILDCARD
    private def sameName(name: Name, other: Name) =  (name eq other) || (name ne null) && name.start == other.start && name.length == other.length
    def hasName(other: Name) = sameName(name, other)
    def introduces(target: Name) =
      if (target == nme.WILDCARD) isLiteralWildcard
      else target != null && !isGiven && sameName(rename, target)
  }
  object ImportSelector extends ImportSelectorExtractor {
    private val wild = ImportSelector(nme.WILDCARD, -1, null, -1)
    val wildList = List(wild) // OPT This list is shared for performance. Used for unpositioned synthetic only.
    def wildAt(pos: Int) = ImportSelector(nme.WILDCARD, pos, null, -1)
    def givenAt(pos: Int) = ImportSelector(nme.WILDCARD, pos, nme.`given`, -1)
    def mask(name: Name) = ImportSelector(name, -1, nme.WILDCARD, -1)
  }

  case class Import(expr: Tree, selectors: List[ImportSelector])
       extends SymTree with ImportApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Import(this, transformer.transform(expr), selectors)
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(expr)
      selectors foreach traverser.traverseImportSelector
    }
    def posOf(sel: ImportSelector): Position = {
      val pos0 = this.pos
      val start = sel.namePos
      if (start >= 0 && selectors.contains(sel)) {
        val hasRename = sel.rename != null && sel.renamePos >= 0 // !sel.isWildcard
        val end = if (hasRename) sel.renamePos + sel.rename.length else start + sel.name.length
        pos0.withStart(start).withEnd(end) ^ start
      }
      else pos0
    }
  }
  object Import extends ImportExtractor

  case class Template(parents: List[Tree], self: ValDef, body: List[Tree])
       extends SymTree with TemplateApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Template(this, transformer.transformTrees(parents), transformer.transformValDef(self), transformer.transformStats(body, this.symbol))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverseParents(parents)
      traverser.traverseSelfType(self)
      traverser.traverseStats(body, this.symbol)
    }
  }
  object Template extends TemplateExtractor

  case class Block(stats: List[Tree], expr: Tree)
       extends TermTree with BlockApi {
    override def transform(transformer: Transformer): Tree =
      treeCopy.Block(this, transformer.transformStats(stats, transformer.currentOwner), transformer.transform(expr))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverseTrees(stats)
      traverser.traverse(expr)
    }
  }
  object Block extends BlockExtractor

  case class CaseDef(pat: Tree, guard: Tree, body: Tree)
       extends Tree with CaseDefApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.CaseDef(this, transformer.transform(pat), transformer.transform(guard), transformer.transform(body))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traversePattern(pat)
      traverser.traverseGuard(guard)
      traverser.traverse(body)
    }
  }
  object CaseDef extends CaseDefExtractor

  case class Alternative(trees: List[Tree])
       extends TermTree with AlternativeApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Alternative(this, transformer.transformTrees(trees))
    override def traverse(traverser: Traverser): Unit =
      traverser.traverseTrees(trees)
  }
  object Alternative extends AlternativeExtractor

  case class Star(elem: Tree)
       extends TermTree with StarApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Star(this, transformer.transform(elem))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(elem)
    }
  }
  object Star extends StarExtractor

  case class Bind(name: Name, body: Tree)
       extends DefTree with BindApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Bind(this, name, transformer.transform(body))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverseName(name)
      traverser.traverse(body)
    }
  }
  object Bind extends BindExtractor

  case class UnApply(fun: Tree, args: List[Tree])
       extends TermTree with UnApplyApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.UnApply(this, transformer.transform(fun), transformer.transformTrees(args)) // bq: see test/.../unapplyContexts2.scala
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(fun)
      traverser.traverseTrees(args)
    }
  }
  object UnApply extends UnApplyExtractor

  /** An array of expressions. This AST node needs to be translated in backend.
   *  It is used to pass arguments to vararg arguments.
   *  Introduced by compiler phase uncurry.
   *
   *  This AST node does not have direct correspondence to Scala code,
   *  and is used to pass arguments to vararg arguments. For instance:
   *
   *    printf("%s%d", foo, 42)
   *
   *  Is translated to after compiler phase uncurry to:
   *
   *    Apply(
   *      Ident("printf"),
   *      Literal("%s%d"),
   *      ArrayValue(<Any>, List(Ident("foo"), Literal(42))))
   */
  case class ArrayValue(elemtpt: Tree, elems: List[Tree]) extends TermTree {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.ArrayValue(this, transformer.transform(elemtpt), transformer.transformTrees(elems))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(elemtpt)
      traverser.traverseTrees(elems)
    }
  }

  case class Function(vparams: List[ValDef], body: Tree)
       extends SymTree with TermTree with FunctionApi {
    override def transform(transformer: Transformer): Tree =
      transformer.atOwner(this.symbol) {
        transformer.treeCopy.Function(this, transformer.transformValDefs(vparams), transformer.transform(body))
      }
    override def traverse(traverser: Traverser): Unit = traverser.atOwner(this.symbol) {
      traverser.traverseParams(vparams) ; traverser.traverse(body)
    }
  }
  object Function extends FunctionExtractor

  case class Assign(lhs: Tree, rhs: Tree)
       extends TermTree with AssignApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Assign(this, transformer.transform(lhs), transformer.transform(rhs))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(lhs)
      traverser.traverse(rhs)
    }
  }
  object Assign extends AssignExtractor

  case class NamedArg(lhs: Tree, rhs: Tree)
       extends TermTree with NamedArgApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.NamedArg(this, transformer.transform(lhs), transformer.transform(rhs))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(lhs)
      traverser.traverse(rhs)
    }
  }
  object NamedArg extends NamedArgExtractor

  case class If(cond: Tree, thenp: Tree, elsep: Tree)
       extends TermTree with IfApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.If(this, transformer.transform(cond), transformer.transform(thenp), transformer.transform(elsep))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(cond)
      traverser.traverse(thenp)
      traverser.traverse(elsep)
    }
  }
  object If extends IfExtractor

  case class Match(selector: Tree, cases: List[CaseDef])
       extends TermTree with MatchApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Match(this, transformer.transform(selector), transformer.transformCaseDefs(cases))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(selector)
      traverser.traverseCases(cases)
    }
  }
  object Match extends MatchExtractor

  case class Return(expr: Tree)
       extends SymTree with TermTree with ReturnApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Return(this, transformer.transform(expr))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(expr)
    }
  }
  object Return extends ReturnExtractor

  case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)
       extends TermTree with TryApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Try(this, transformer.transform(block), transformer.transformCaseDefs(catches), transformer.transform(finalizer))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(block)
      traverser.traverseCases(catches)
      traverser.traverse(finalizer)
    }
  }
  object Try extends TryExtractor

  case class Throw(expr: Tree)
       extends TermTree with ThrowApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Throw(this, transformer.transform(expr))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(expr)
    }
  }
  object Throw extends ThrowExtractor

  case class New(tpt: Tree) extends TermTree with NewApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.New(this, transformer.transform(tpt))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(tpt)
    }
  }
  object New extends NewExtractor

  case class Typed(expr: Tree, tpt: Tree)
       extends TermTree with TypedApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Typed(this, transformer.transform(expr), transformer.transform(tpt))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(expr)
      traverser.traverseTypeAscription(tpt)
    }
  }
  object Typed extends TypedExtractor

  // represents `expr _`, as specified in Method Values of spec/06-expressions.md
  object MethodValue {
    def apply(expr: Tree): Tree = Typed(expr, Function(Nil, EmptyTree))
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Typed(expr, Function(Nil, EmptyTree)) => Some(expr)
      case _ => None
    }
  }

  abstract class GenericApply extends TermTree with GenericApplyApi {
    val fun: Tree
    val args: List[Tree]
  }

  case class TypeApply(fun: Tree, args: List[Tree])
       extends GenericApply with TypeApplyApi {

    assert(fun.isTerm, fun)

    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol): Unit = { fun.symbol = sym }
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.TypeApply(this, transformer.transform(fun), transformer.transformTrees(args))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(fun)
      traverser.traverseTypeArgs(args)
    }
  }
  object TypeApply extends TypeApplyExtractor

  case class Apply(fun: Tree, args: List[Tree])
       extends GenericApply with ApplyApi {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol): Unit = { fun.symbol = sym }
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Apply(this, transformer.transform(fun), transformer.transformTrees(args))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(fun)
      traverser.traverseTrees(args)
    }
  }
  object Apply extends ApplyExtractor

  // TODO remove this class, add a tree attachment to Apply to track whether implicits were involved
  // copying trees will all too easily forget to distinguish subclasses
  class ApplyToImplicitArgs(fun: Tree, args: List[Tree]) extends Apply(fun, args)

  // TODO remove this class, add a tree attachment to Apply to track whether implicits were involved
  // copying trees will all too easily forget to distinguish subclasses
  class ApplyImplicitView(fun: Tree, args: List[Tree]) extends Apply(fun, args)

  def ApplyConstructor(tpt: Tree, args: List[Tree]) = Apply(Select(New(tpt), nme.CONSTRUCTOR), args)

  // Creates a constructor call from the constructor symbol.  This is
  // to avoid winding up with an OverloadedType for the constructor call.
  def NewFromConstructor(constructor: Symbol, args: Tree*) = {
    assert(constructor.isConstructor, constructor)
    val instance = New(TypeTree(constructor.owner.tpe))
    val init     = Select(instance, nme.CONSTRUCTOR) setSymbol constructor

    Apply(init, args.toList)
  }

  case class ApplyDynamic(qual: Tree, args: List[Tree]) extends SymTree with TermTree {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.ApplyDynamic(this, transformer.transform(qual), transformer.transformTrees(args))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(qual)
      traverser.traverseTrees(args)
    }
  }

  case class Super(qual: Tree, mix: TypeName) extends TermTree with SuperApi {
    override def symbol: Symbol = qual.symbol
    override def symbol_=(sym: Symbol): Unit = { qual.symbol = sym }
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Super(this, transformer.transform(qual), mix)
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(qual)
      traverser.traverseName(mix)
    }
  }
  object Super extends SuperExtractor

  case class This(qual: TypeName)
        extends SymTree with TermTree with ThisApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.This(this, qual)
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverseName(qual)
    }
  }
  object This extends ThisExtractor

  case class Select(qualifier: Tree, name: Name)
       extends RefTree with SelectApi {

    // !!! assert disabled due to test case pos/annotDepMethType.scala triggering it.
    // assert(qualifier.isTerm, qualifier)

    override def transform(transformer: Transformer): Tree = {
      transformer.treeCopy.Select(this, transformer.transform(qualifier), name)
    }
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(qualifier)
      traverser.traverseName(name)
    }
  }
  object Select extends SelectExtractor

  case class Ident(name: Name) extends RefTree with IdentApi {
    def qualifier: Tree = EmptyTree
    def isBackquoted = this.hasAttachment[BackquotedIdentifierAttachment.type]
    override def transform(transformer: Transformer): Tree = {
      transformer.treeCopy.Ident(this, name)
    }
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverseName(name)
    }
  }
  object Ident extends IdentExtractor

  case class ReferenceToBoxed(ident: Ident) extends TermTree with ReferenceToBoxedApi {
    override def symbol: Symbol = ident.symbol
    override def symbol_=(sym: Symbol): Unit = { ident.symbol = sym }
    override def transform(transformer: Transformer): Tree = {
      transformer.treeCopy.ReferenceToBoxed(this, transformer.transform(ident) match {
        case idt1: Ident => idt1
        case x           => throw new MatchError(x)
      })
    }
    override def traverse(traverser: Traverser): Unit = traverser.traverse(ident)
  }
  object ReferenceToBoxed extends ReferenceToBoxedExtractor

  case class Literal(value: Constant)
        extends TermTree with LiteralApi {
    assert(value ne null, "null value for literal")
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Literal(this, value)
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverseConstant(value)
    }
  }
  object Literal extends LiteralExtractor

//  @deprecated("will be removed and then be re-introduced with changed semantics, use Literal(Constant(x)) instead")
//  def Literal(x: Any) = new Literal(Constant(x))

  case class Annotated(annot: Tree, arg: Tree) extends Tree with AnnotatedApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.Annotated(this, transformer.transform(annot), transformer.transform(arg))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(annot)
      traverser.traverse(arg)
    }
  }
  object Annotated extends AnnotatedExtractor

  case class SingletonTypeTree(ref: Tree)
        extends TypTree with SingletonTypeTreeApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.SingletonTypeTree(this, transformer.transform(ref))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(ref)
    }
  }
  object SingletonTypeTree extends SingletonTypeTreeExtractor

  case class SelectFromTypeTree(qualifier: Tree, name: TypeName)
       extends RefTree with TypTree with SelectFromTypeTreeApi {

    assert(qualifier.isType, qualifier)
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.SelectFromTypeTree(this, transformer.transform(qualifier), name)
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(qualifier)
      traverser.traverseName(name)
    }
  }
  object SelectFromTypeTree extends SelectFromTypeTreeExtractor

  case class CompoundTypeTree(templ: Template)
       extends TypTree with CompoundTypeTreeApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.CompoundTypeTree(this, transformer.transformTemplate(templ))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(templ)
    }
  }
  object CompoundTypeTree extends CompoundTypeTreeExtractor

  case class AppliedTypeTree(tpt: Tree, args: List[Tree])
       extends TypTree with AppliedTypeTreeApi {

    assert(tpt.isType, tpt)

    override def symbol: Symbol = tpt.symbol
    override def symbol_=(sym: Symbol): Unit = { tpt.symbol = sym }
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.AppliedTypeTree(this, transformer.transform(tpt), transformer.transformTrees(args))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(tpt)
      traverser.traverseTypeArgs(args)
    }
  }
  object AppliedTypeTree extends AppliedTypeTreeExtractor

  case class TypeBoundsTree(lo: Tree, hi: Tree)
       extends TypTree with TypeBoundsTreeApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.TypeBoundsTree(this, transformer.transform(lo), transformer.transform(hi))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(lo)
      traverser.traverse(hi)
    }
  }
  object TypeBoundsTree extends TypeBoundsTreeExtractor

  case class ExistentialTypeTree(tpt: Tree, whereClauses: List[MemberDef])
       extends TypTree with ExistentialTypeTreeApi {
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.ExistentialTypeTree(this, transformer.transform(tpt), transformer.transformMemberDefs(whereClauses))
    override def traverse(traverser: Traverser): Unit = {
      traverser.traverse(tpt)
      traverser.traverseTrees(whereClauses)
    }
  }
  object ExistentialTypeTree extends ExistentialTypeTreeExtractor

  case class TypeTree() extends TypTree with TypeTreeApi {
    private var orig: Tree = null
    /** Was this type tree originally empty? That is, does it now contain
      * an inferred type that must be forgotten in `resetAttrs` to
      * enable retyping.
      */
    private[scala] var wasEmpty: Boolean = false

    override def symbol = typeTreeSymbol(this) // if (tpe == null) null else tpe.typeSymbol
    override def isEmpty = (tpe eq null) || tpe == NoType

    def original: Tree = orig
    def setOriginal(tree: Tree): this.type = {
      @tailrec
      def followOriginal(t: Tree): Tree = t match {
        case tt: TypeTree => followOriginal(tt.original)
        case t => t
      }

      orig = followOriginal(tree); setPos(tree.pos)
      this
    }

    override def defineType(tp: Type): this.type = {
      wasEmpty = isEmpty
      setType(tp)
    }

    override private[scala] def copyAttrs(tree: Tree) = {
      super.copyAttrs(tree)
      tree match {
        case other: TypeTree =>
          // scala/bug#6648 Critical for correct operation of `resetAttrs`.
          wasEmpty = other.wasEmpty
          if (other.orig != null)
            orig = other.orig.duplicate
        case _ =>
      }
      this
    }
    override def transform(transformer: Transformer): Tree =
      transformer.treeCopy.TypeTree(this)
    override def traverse(traverser: Traverser): Unit =
      ()

  }
  object TypeTree extends TypeTreeExtractor

  def TypeTree(tp: Type): TypeTree = TypeTree() setType tp
  private def TypeTreeMemberType(sym: Symbol): TypeTree = {
    // Needed for pos/t4970*.scala. See scala/bug#7853
    val resType = (if (sym.isLocalToBlock) sym.tpe else (sym.owner.thisType memberType sym)).finalResultType
    atPos(sym.pos.focus)(TypeTree(resType))
  }

  def TypeBoundsTree(bounds: TypeBounds): TypeBoundsTree = TypeBoundsTree(TypeTree(bounds.lo), TypeTree(bounds.hi))
  def TypeBoundsTree(sym: Symbol): TypeBoundsTree        = atPos(sym.pos)(TypeBoundsTree(sym.info.bounds))

  override type TreeCopier >: Null <: InternalTreeCopierOps
  abstract class InternalTreeCopierOps extends TreeCopierOps {
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]): ApplyDynamic
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]): ArrayValue
  }

  class StrictTreeCopier extends InternalTreeCopierOps {
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
    def NamedArg(tree: Tree, lhs: Tree, rhs: Tree) =
      new NamedArg(lhs, rhs).copyAttrs(tree)
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
        case self.pendingSuperCall => self.pendingSuperCall
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
    def RefTree(tree: Tree, qualifier: Tree, selector: Name) =
      self.RefTree(qualifier, selector) copyAttrs tree
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
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[MemberDef]) =
      new ExistentialTypeTree(tpt, whereClauses).copyAttrs(tree)
  }

  class LazyTreeCopier extends InternalTreeCopierOps {
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
    def NamedArg(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ NamedArg(lhs0, rhs0)
      if (lhs0 == lhs) && (rhs0 == rhs) => t
      case _ => treeCopy.NamedArg(tree, lhs, rhs)
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
    def RefTree(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ Select(qualifier0, selector0)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => treeCopy.RefTree(tree, qualifier, selector)
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
      if (annot0==annot && arg0==arg) => t
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
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[MemberDef]) = tree match {
      case t @ ExistentialTypeTree(tpt0, whereClauses0)
      if (tpt0 == tpt) && (whereClauses0 == whereClauses) => t
      case _ => treeCopy.ExistentialTypeTree(tree, tpt, whereClauses)
    }
  }

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
    def | (flag: Int): Modifiers = this | flag.toLong
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

    override def mapAnnotations(f: List[Tree] => List[Tree]): Modifiers = {
      val newAnns = f(annotations)
      if (annotations == newAnns) this
      else Modifiers(flags, privateWithin, newAnns) setPositions positions
    }

    override def toString = s"Modifiers($flagString, ${annotations.mkString(",")}, $positions)"
  }

  object Modifiers extends ModifiersExtractor

  implicit val ModifiersTag: ClassTag[Modifiers] = ClassTag[Modifiers](classOf[Modifiers])

  // ---- values and creators ---------------------------------------

  /** @param sym       the template's symbol
   *  @param body      trees that constitute the body of the template
   *  @return          the template
   */
  def Template(sym: Symbol, body: List[Tree]): Template = {
    atPos(sym.pos) {
      Template(sym.info.parents map TypeTree,
               if (sym.thisSym == sym) noSelfType else ValDef(sym),
               body)
    }
  }

  trait CannotHaveAttrs extends Tree {
    super.setPos(NoPosition)
    super.setType(NoType)

    override def canHaveAttrs = false
    override def setPos(pos: Position): this.type = { requireLegal(pos, NoPosition, "pos"); this }
    override def pos_=(pos: Position) = setPos(pos)
    override def setType(t: Type) = { requireLegal(t, NoType, "tpe"); this }
    override def tpe_=(t: Type) = setType(t)

    // We silently ignore attempts to add attachments to `EmptyTree`. See scala/bug#8947 for an
    // example of a bug in macro expansion that this solves.
    override def setAttachments(attachments: Attachments {type Pos = Position}): this.type = attachmentWarning()
    override def updateAttachment[T: ClassTag](attachment: T): this.type = attachmentWarning()
    override def removeAttachment[T: ClassTag]: this.type = attachmentWarning()
    private def attachmentWarning(): this.type = {devWarning(s"Attempt to mutate attachments on $self ignored"); this}

    private def requireLegal(value: Any, allowed: Any, what: String): Unit =
      if (value != allowed && this != pendingSuperCall) {
        log(s"can't set $what for $self to value other than $allowed")
        if (settings.isDebug && settings.isDeveloper)
          new Throwable(s"can't set $what for $self to value other than $allowed").printStackTrace
      }
    override def traverse(traverser: Traverser): Unit = ()
  }

  case object EmptyTree extends TermTree with CannotHaveAttrs {
    override def isEmpty = true
    val asList = List(this)
    override def transform(transformer: Transformer): Tree = this
  }
  object noSelfType extends ValDef(Modifiers(PRIVATE), nme.WILDCARD, TypeTree(NoType), EmptyTree) with CannotHaveAttrs
  object pendingSuperCall extends Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List()) with CannotHaveAttrs

  @deprecated("use `noSelfType` instead", "2.11.0") lazy val emptyValDef = noSelfType

  class InternalTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree.transform(this)
  }
  class InternalTraverser extends Traverser {
    override def traverse(tree: Tree): Unit = tree.traverse(this)
    override def apply[T <: Tree](tree: T): tree.type = super.apply(tree)
  }

  def newValDef(sym: Symbol, rhs: Tree)(
    mods: Modifiers = Modifiers(sym.flags),
    name: TermName  = sym.name.toTermName,
    tpt: Tree       = TypeTreeMemberType(sym)
  ): ValDef = (
    atPos(sym.pos)(ValDef(mods, name, tpt, rhs)) setSymbol sym
  )

  def newDefDef(sym: Symbol, rhs: Tree)(
    mods: Modifiers              = Modifiers(sym.flags),
    name: TermName               = sym.name.toTermName,
    tparams: List[TypeDef]       = sym.typeParams map TypeDef.apply,
    vparamss: List[List[ValDef]] = mapParamss(sym)(ValDef.apply),
    tpt: Tree                    = TypeTreeMemberType(sym)
  ): DefDef = (
    atPos(sym.pos)(DefDef(mods, name, tparams, vparamss, tpt, rhs)) setSymbol sym
  )
  def newDefDefAt(pos: Position)(sym: Symbol, rhs: Tree)(
    mods: Modifiers              = Modifiers(sym.flags),
    name: TermName               = sym.name.toTermName,
    tparams: List[TypeDef]       = sym.typeParams map TypeDef.apply,
    vparamss: List[List[ValDef]] = mapParamss(sym)(ValDef.apply),
    tpt: Tree                    = TypeTreeMemberType(sym)
  ): DefDef =
    atPos(pos)(DefDef(mods, name, tparams, vparamss, tpt, rhs)).setSymbol(sym)

  def newTypeDef(sym: Symbol, rhs: Tree)(
    mods: Modifiers        = Modifiers(sym.flags),
    name: TypeName         = sym.name.toTypeName,
    tparams: List[TypeDef] = sym.typeParams map TypeDef.apply
  ): TypeDef = (
    atPos(sym.pos)(TypeDef(mods, name, tparams, rhs)) setSymbol sym
  )

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
    case xs :: rest => rest.foldLeft(ApplyConstructor(tpt, xs): Tree)(Apply.apply)
  }

  /** 0-1 argument list new, based on a type.
   */
  def New(tpe: Type, args: Tree*): Tree =
    ApplyConstructor(TypeTree(tpe), args.toList)

  def New(tpe: Type, argss: List[List[Tree]]): Tree =
    New(TypeTree(tpe), argss)

  def New(sym: Symbol, args: Tree*): Tree =
    New(sym.tpe, args: _*)

  def Super(sym: Symbol, mix: TypeName): Tree =
    Super(This(sym), mix)

  /**
   * Creates a tree that selects a specific member `sym` without having to qualify the `super`.
   * For example, given traits `B <:< A`, a class `C <:< B` needs to invoke `A.\$init\$`. If `A` is
   * not a direct parent, a tree `super[A].\$init\$` would not type check ("does not name a parent").
   * So we generate `super.\$init\$` and pre-assign the correct symbol. A special-case in
   * `typedSelectInternal` assigns the correct type `A` to the `super` qualifier.
   */
  def SuperSelect(clazz: Symbol, sym: Symbol): Tree = {
    Select(Super(clazz, tpnme.EMPTY), sym).updateAttachment(new QualTypeSymAttachment(sym.owner))
  }

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
  def Block(stats: Tree*): Block = stats match {
    case Seq(b @ Block(_, _)) => b
    case init :+ last         => Block(init.toList, last)
    case _                    => Block(stats.toList, Literal(Constant(())))
  }

  /** Delegate for a TypeTree symbol. This operation is unsafe because
   *  it may trigger type checking when forcing the type symbol of the
   *  underlying type.
   */
  protected def typeTreeSymbol(tree: TypeTree): Symbol =
    if (tree.tpe == null) null else tree.tpe.typeSymbol

  // --- generic traversers and transformers

  @deprecated("Use Tree#traverse instead", since = "2.12.3")
  override protected def itraverse(traverser: Traverser, tree: Tree): Unit = {
    tree.traverse(traverser)
  }

  //OPT ordered according to frequency to speed it up.
  @deprecated("Use Tree#transform instead", since = "2.12.3")
  override protected def itransform(transformer: Transformer, tree: Tree): Tree = {
    tree.transform(transformer)
  }

  private def mclass(sym: Symbol) = sym map (_.asModule.moduleClass)

  // --- specific traversers and transformers

  class ForeachPartialTreeTraverser(pf: PartialFunction[Tree, Tree]) extends InternalTraverser {
    override def traverse(tree: Tree): Unit = {
      val t = if (pf isDefinedAt tree) pf(tree) else tree
      super.traverse(t)
    }
  }

  class ChangeOwnerTraverser(val oldowner: Symbol, val newowner: Symbol) extends InternalTraverser {
    protected val changedSymbols = mutable.Set.empty[Symbol]
    protected val treeTypes = mutable.Set.empty[Type]

    def change(sym: Symbol): Unit = {
      if (sym != NoSymbol && sym.owner == oldowner) {
        sym.owner = newowner
        changedSymbols += sym
        if (sym.isModule) {
          sym.moduleClass.owner = newowner
          changedSymbols += sym.moduleClass
        }
      }
    }

    override def apply[T <: Tree](tree: T): tree.type = {
      traverse(tree)
      if (changedSymbols.nonEmpty)
        new InvalidateTypeCaches(changedSymbols).invalidate(treeTypes)
      tree
    }

    override def traverse(tree: Tree): Unit = {
      if (tree.tpe != null) treeTypes += tree.tpe
      tree match {
        case _: Return =>
          if (tree.symbol == oldowner) {
            // scala/bug#5612
            if (newowner hasTransOwner oldowner)
              log("NOT changing owner of %s because %s is nested in %s".format(tree, newowner, oldowner))
            else {
              log("changing owner of %s: %s => %s".format(tree, oldowner, newowner))
              tree.symbol = newowner
            }
          }
        case _: DefTree | _: Function =>
          change(tree.symbol)
        case _ =>
      }
      tree.traverse(this)
    }
  }

  class LocalOwnersTraverser extends InternalTraverser {
    val result: mutable.Set[Symbol] = mutable.Set.empty[Symbol]

    override def traverse(tree: Tree): Unit = {
      tree match {
        case _: DefTree | _: Function if(tree.hasExistingSymbol) =>
          result += tree.symbol
        case _ =>
      }
      tree.traverse(this)
    }
  }

  def changeNonLocalOwners(tree: Tree, newowner: Symbol): Unit = {
    val localOwnersTraverser = new LocalOwnersTraverser
    localOwnersTraverser(tree)
    val localOwners = localOwnersTraverser.result
    localOwners.foreach { sym =>
      if (!localOwners.contains(sym.owner)) {
        sym.owner = newowner
        if (sym.isModule) sym.moduleClass.owner = newowner
      }
    }
  }

  private class ShallowDuplicator(orig: Tree) extends InternalTransformer {
    override val treeCopy = newStrictTreeCopier
    override def transform(tree: Tree) =
      if (tree eq orig) tree.transform(this)
      else tree
  }

  /** A transformer that replaces tree `from` with tree `to` in a given tree */
  class TreeReplacer(from: Tree, to: Tree, positionAware: Boolean) extends InternalTransformer {
    override def transform(t: Tree): Tree = {
      if (t == from) to
      else if (!positionAware || (t.pos includes from.pos) || t.pos.isTransparent) super.transform(t)
      else t
    }
  }

  // Create a readable string describing a substitution.
  private def substituterString(fromStr: String, toStr: String, from: List[Any], to: List[Any]): String = {
    val toAndFro = from.lazyZip(to).map((f, t) => s"$f -> $t").mkString("(", ", ", ")")
    s"subst[$fromStr, $toStr]$toAndFro"
  }

  // NOTE: calls shallowDuplicate on trees in `to` to avoid problems when symbols in `from`
  // occur multiple times in the `tree` passed to `transform`,
  // otherwise, the resulting Tree would be a graph, not a tree... this breaks all sorts of stuff,
  // notably concerning the mutable aspects of Trees (such as setting their .tpe)
  class TreeSubstituter(from: List[Symbol], to: List[Tree]) extends InternalTransformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(_) =>
        @tailrec
        def subst(from: List[Symbol], to: List[Tree]): Tree =
          if (from.isEmpty) tree
          else if (tree.symbol == from.head) to.head.shallowDuplicate // TODO: does it ever make sense *not* to perform a shallowDuplicate on `to.head`?
          else subst(from.tail, to.tail)
        subst(from, to)
      case _ =>
        super.transform(tree)
    }
    override def toString = substituterString("Symbol", "Tree", from, to)
  }

  /** Substitute clazz.this with `to`. `to` must be an attributed tree.
   */
  class ThisSubstituter(clazz: Symbol, to: => Tree) extends InternalTransformer {
    val newtpe = to.tpe
    override def transform(tree: Tree) = {
      tree modifyType (_.substThis(clazz, newtpe))
      tree match {
        case This(_) if tree.symbol == clazz => to
        case _ => tree.transform(this)
      }
    }
  }

  class TypeMapTreeSubstituter(val typeMap: TypeMap) extends InternalTraverser {
    override def traverse(tree: Tree): Unit = {
      tree modifyType typeMap
      if (tree.isDef)
        tree.symbol modifyInfo typeMap

      tree.traverse(this)
    }
    override def apply[T <: Tree](tree: T): tree.type = super.apply(tree.duplicate)
  }

  class TreeTypeSubstituter(val from: List[Symbol], val to: List[Type]) extends TypeMapTreeSubstituter(new SubstTypeMap(from, to)) {
    def isEmpty = from.isEmpty && to.isEmpty
    override def toString() = "TreeTypeSubstituter("+from+","+to+")"
  }

  lazy val EmptyTreeTypeSubstituter = new TreeTypeSubstituter(List(), List())

  class TreeSymSubstTraverser(val from: List[Symbol], val to: List[Symbol]) extends TypeMapTreeSubstituter(SubstSymMap(from, to)) {
    override def toString() = "TreeSymSubstTraverser/" + substituterString("Symbol", "Symbol", from, to)
  }

  /** Substitute symbols in `from` with symbols in `to`. Returns a new
   *  tree using the new symbols and whose Ident and Select nodes are
   *  name-consistent with the new symbols.
   *
   *  Note: This is currently a destructive operation on the original Tree.
   *  Trees currently assigned a symbol in `from` will be assigned the new symbols
   *  without copying, and trees that define symbols with an `info` that refer
   *  a symbol in `from` will have a new type assigned.
   */
  class TreeSymSubstituter(from: List[Symbol], to: List[Symbol]) extends InternalTransformer {
    val symSubst = SubstSymMap(from, to)

    protected val changedSymbols = mutable.Set.empty[Symbol]
    protected val treeTypes = mutable.Set.empty[Type]

    override def transform(tree: Tree): Tree = {
      @tailrec
      def subst(from: List[Symbol], to: List[Symbol]): Unit = {
        if (!from.isEmpty)
          if (tree.symbol == from.head) tree setSymbol to.head
          else subst(from.tail, to.tail)
      }
      tree modifyType symSubst
      if (tree.tpe != null) treeTypes += tree.tpe

      if (tree.hasSymbolField) {
        subst(from, to)
        tree match {
          case _: DefTree =>
            def update(sym: Symbol) = {
              val newInfo = symSubst(sym.info)
              if (!(newInfo =:= sym.info)) {
                debuglog(sm"""
                  |TreeSymSubstituter: updated info of symbol ${sym}
                  |  Old: ${showRaw(sym.info, printTypes = true, printIds = true)}
                  |  New: ${showRaw(newInfo, printTypes = true, printIds = true)}""")
                changedSymbols += sym
                sym updateInfo newInfo
              }
            }
            update(tree.symbol)
            if (tree.symbol.isModule) update(tree.symbol.moduleClass)
          case _          =>
            // no special handling is required for Function or Import nodes here.
            // as they don't have interesting infos attached to their symbols.
            // Substitution of the referenced symbol of Return nodes is handled
            // in .ChangeOwnerTraverser
        }
        tree match {
          case Ident(name0) if tree.symbol != NoSymbol =>
            treeCopy.Ident(tree, tree.symbol.name)
          case Select(qual, name0) if tree.symbol != NoSymbol =>
            treeCopy.Select(tree, transform(qual), tree.symbol.name)
          case _ =>
            tree.transform(this)
        }
      } else
        super.transform(tree)
    }
    def apply[T <: Tree](tree: T): T = {
      val tree1 = transform(tree)
      if (changedSymbols.nonEmpty)
        new InvalidateTypeCaches(changedSymbols).invalidate(treeTypes)
      tree1.asInstanceOf[T]
    }
    override def toString() = "TreeSymSubstituter/" + substituterString("Symbol", "Symbol", from, to)
  }


  class ForeachTreeTraverser(f: Tree => Unit) extends InternalTraverser {
    override def traverse(t: Tree): Unit = {
      f(t)
      t.traverse(this)
    }
  }

  class FilterTreeTraverser(p: Tree => Boolean) extends InternalTraverser {
    val hits = mutable.ListBuffer[Tree]()
    override def traverse(t: Tree): Unit = {
      if (p(t)) hits += t
      t.traverse(this)
    }
  }

  class CollectTreeTraverser[T](pf: PartialFunction[Tree, T]) extends InternalTraverser {
    val results = mutable.ListBuffer[T]()
    override def traverse(t: Tree): Unit = {
      if (pf.isDefinedAt(t)) results += pf(t)
      t.traverse(this)
    }
  }

  class FindTreeTraverser(p: Tree => Boolean) extends InternalTraverser {
    var result: Option[Tree] = None
    override def traverse(t: Tree): Unit = {
      if (result.isEmpty) {
        if (p(t)) result = Some(t) else t.traverse(this)
      }
    }
  }

  private lazy val duplicator = new Duplicator(focusPositions = true)
  private class Duplicator(focusPositions: Boolean) extends InternalTransformer {
    override val treeCopy = newStrictTreeCopier
    override def transform(t: Tree) = {
      val t1 = t.transform(this)
      if ((t1 ne t) && t1.pos.isRange && focusPositions) t1 setPos t.pos.focus
      t1
    }
  }
  object duplicateAndResetPos extends Transformer {
    override val treeCopy = newStrictTreeCopier
    override def transform(t: Tree) = {
      val t1 = super.transform(t)
      if (t1 ne EmptyTree) t1.setPos(NoPosition)
      t1
    }
  }

  final def focusInPlace(t: Tree): t.type =
    if (useOffsetPositions) t else { focuser traverse t; t }
  private object focuser extends InternalTraverser {
    override def traverse(t: Tree) = {
      t setPos t.pos.focus
      t traverse this
    }
  }

  trait TreeStackTraverser extends Traverser {
    var path: List[Tree] = Nil
    abstract override def traverse(t: Tree) = {
      path ::= t
      try super.traverse(t)
      finally path = path.tail
    }
  }

  def duplicateAndKeepPositions(tree: Tree) = new Duplicator(focusPositions = false) transform tree

  // this is necessary to avoid crashes like https://github.com/scalamacros/paradise/issues/1
  // when someone tries to c.typecheck a naked MemberDef
  def wrappingIntoTerm(tree0: Tree)(op: Tree => Tree): Tree = {
    val neededWrapping = !tree0.isTerm
    val tree1 = build.SyntacticBlock(tree0 :: Nil)
    op(tree1) match {
      case Block(tree2 :: Nil, Literal(Constant(()))) if neededWrapping => tree2
      case tree2 => tree2
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
      throw new IllegalStateException("Not a DefDef: " + t + "/" + t.getClass)
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
      throw new IllegalStateException("Not a ValDef: " + t + "/" + t.getClass)
  }
  def copyTypeDef(tree: Tree)(
    mods: Modifiers        = null,
    name: Name             = null,
    tparams: List[TypeDef] = null,
    rhs: Tree              = null
  ): TypeDef = tree match {
    case TypeDef(mods0, name0, tparams0, rhs0) =>
      treeCopy.TypeDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (tparams eq null) tparams0 else tparams,
        if (rhs eq null) rhs0 else rhs
      )
    case t =>
      throw new IllegalStateException("Not a TypeDef: " + t + "/" + t.getClass)
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
      throw new IllegalStateException("Not a ClassDef: " + t + "/" + t.getClass)
  }

  def copyModuleDef(tree: Tree)(
    mods: Modifiers        = null,
    name: Name             = null,
    impl: Template         = null
  ): ModuleDef = tree match {
    case ModuleDef(mods0, name0, impl0) =>
      treeCopy.ModuleDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (impl eq null) impl0 else impl
      )
    case t =>
      throw new IllegalStateException("Not a ModuleDef: " + t + "/" + t.getClass)
  }

  def deriveDefDef(ddef: Tree)(applyToRhs: Tree => Tree): DefDef = ddef match {
    case DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0) =>
      treeCopy.DefDef(ddef, mods0, name0, tparams0, vparamss0, tpt0, applyToRhs(rhs0))
    case t =>
      throw new IllegalStateException("Not a DefDef: " + t + "/" + t.getClass)
  }
  def deriveValDef(vdef: Tree)(applyToRhs: Tree => Tree): ValDef = vdef match {
    case ValDef(mods0, name0, tpt0, rhs0) =>
      treeCopy.ValDef(vdef, mods0, name0, tpt0, applyToRhs(rhs0))
    case t =>
      throw new IllegalStateException("Not a ValDef: " + t + "/" + t.getClass)
  }
  def deriveTemplate(templ: Tree)(applyToBody: List[Tree] => List[Tree]): Template = templ match {
    case Template(parents0, self0, body0) =>
      treeCopy.Template(templ, parents0, self0, applyToBody(body0))
    case t =>
      throw new IllegalStateException("Not a Template: " + t + "/" + t.getClass)
  }
  def deriveClassDef(cdef: Tree)(applyToImpl: Template => Template): ClassDef = cdef match {
    case ClassDef(mods0, name0, tparams0, impl0) =>
      treeCopy.ClassDef(cdef, mods0, name0, tparams0, applyToImpl(impl0))
    case t =>
      throw new IllegalStateException("Not a ClassDef: " + t + "/" + t.getClass)
  }
  def deriveModuleDef(mdef: Tree)(applyToImpl: Template => Template): ModuleDef = mdef match {
    case ModuleDef(mods0, name0, impl0) =>
      treeCopy.ModuleDef(mdef, mods0, name0, applyToImpl(impl0))
    case t =>
      throw new IllegalStateException("Not a ModuleDef: " + t + "/" + t.getClass)
  }
  def deriveCaseDef(cdef: Tree)(applyToBody: Tree => Tree): CaseDef = cdef match {
    case CaseDef(pat0, guard0, body0) =>
      treeCopy.CaseDef(cdef, pat0, guard0, applyToBody(body0))
    case t =>
      throw new IllegalStateException("Not a CaseDef: " + t + "/" + t.getClass)
  }
  def deriveLabelDef(ldef: Tree)(applyToRhs: Tree => Tree): LabelDef = ldef match {
    case LabelDef(name0, params0, rhs0) =>
      treeCopy.LabelDef(ldef, name0, params0, applyToRhs(rhs0))
    case t =>
      throw new IllegalStateException("Not a LabelDef: " + t + "/" + t.getClass)
  }
  def deriveFunction(func: Tree)(applyToRhs: Tree => Tree): Function = func match {
    case Function(params0, rhs0) =>
      treeCopy.Function(func, params0, applyToRhs(rhs0))
    case t =>
      throw new IllegalStateException("Not a Function: " + t + "/" + t.getClass)
  }

  private final class OnlyChildAccumulator extends (Tree => Boolean) {
    private[this] var only: Tree = _
    def apply(t: Tree): Boolean = {
      if (only == null) {
        only = t
        true
      } else {
        only = null
        false // stop traversal
      }
    }
    def result(): Tree = {
      if (only == null) EmptyTree
      else {
        try only
        finally only = null
      }
    }
  }
  private val onlyChildAccumulator = ReusableInstance[OnlyChildAccumulator](new OnlyChildAccumulator, enabled = isCompilerUniverse)

// -------------- Classtags --------------------------------------------------------

  implicit val AlternativeTag: ClassTag[Alternative]                 = ClassTag[Alternative](classOf[Alternative])
  implicit val AnnotatedTag: ClassTag[Annotated]                     = ClassTag[Annotated](classOf[Annotated])
  implicit val AppliedTypeTreeTag: ClassTag[AppliedTypeTree]         = ClassTag[AppliedTypeTree](classOf[AppliedTypeTree])
  implicit val ApplyTag: ClassTag[Apply]                             = ClassTag[Apply](classOf[Apply])
  implicit val NamedArgTag: ClassTag[NamedArg]                       = ClassTag[NamedArg](classOf[NamedArg])
  implicit val AssignTag: ClassTag[Assign]                           = ClassTag[Assign](classOf[Assign])
  implicit val BindTag: ClassTag[Bind]                               = ClassTag[Bind](classOf[Bind])
  implicit val BlockTag: ClassTag[Block]                             = ClassTag[Block](classOf[Block])
  implicit val CaseDefTag: ClassTag[CaseDef]                         = ClassTag[CaseDef](classOf[CaseDef])
  implicit val ClassDefTag: ClassTag[ClassDef]                       = ClassTag[ClassDef](classOf[ClassDef])
  implicit val CompoundTypeTreeTag: ClassTag[CompoundTypeTree]       = ClassTag[CompoundTypeTree](classOf[CompoundTypeTree])
  implicit val DefDefTag: ClassTag[DefDef]                           = ClassTag[DefDef](classOf[DefDef])
  implicit val DefTreeTag: ClassTag[DefTree]                         = ClassTag[DefTree](classOf[DefTree])
  implicit val ExistentialTypeTreeTag: ClassTag[ExistentialTypeTree] = ClassTag[ExistentialTypeTree](classOf[ExistentialTypeTree])
  implicit val FunctionTag: ClassTag[Function]                       = ClassTag[Function](classOf[Function])
  implicit val GenericApplyTag: ClassTag[GenericApply]               = ClassTag[GenericApply](classOf[GenericApply])
  implicit val IdentTag: ClassTag[Ident]                             = ClassTag[Ident](classOf[Ident])
  implicit val IfTag: ClassTag[If]                                   = ClassTag[If](classOf[If])
  implicit val ImplDefTag: ClassTag[ImplDef]                         = ClassTag[ImplDef](classOf[ImplDef])
  implicit val ImportSelectorTag: ClassTag[ImportSelector]           = ClassTag[ImportSelector](classOf[ImportSelector])
  implicit val ImportTag: ClassTag[Import]                           = ClassTag[Import](classOf[Import])
  implicit val LabelDefTag: ClassTag[LabelDef]                       = ClassTag[LabelDef](classOf[LabelDef])
  implicit val LiteralTag: ClassTag[Literal]                         = ClassTag[Literal](classOf[Literal])
  implicit val MatchTag: ClassTag[Match]                             = ClassTag[Match](classOf[Match])
  implicit val MemberDefTag: ClassTag[MemberDef]                     = ClassTag[MemberDef](classOf[MemberDef])
  implicit val ModuleDefTag: ClassTag[ModuleDef]                     = ClassTag[ModuleDef](classOf[ModuleDef])
  implicit val NameTreeTag: ClassTag[NameTree]                       = ClassTag[NameTree](classOf[NameTree])
  implicit val NewTag: ClassTag[New]                                 = ClassTag[New](classOf[New])
  implicit val PackageDefTag: ClassTag[PackageDef]                   = ClassTag[PackageDef](classOf[PackageDef])
  implicit val ReferenceToBoxedTag: ClassTag[ReferenceToBoxed]       = ClassTag[ReferenceToBoxed](classOf[ReferenceToBoxed])
  implicit val RefTreeTag: ClassTag[RefTree]                         = ClassTag[RefTree](classOf[RefTree])
  implicit val ReturnTag: ClassTag[Return]                           = ClassTag[Return](classOf[Return])
  implicit val SelectFromTypeTreeTag: ClassTag[SelectFromTypeTree]   = ClassTag[SelectFromTypeTree](classOf[SelectFromTypeTree])
  implicit val SelectTag: ClassTag[Select]                           = ClassTag[Select](classOf[Select])
  implicit val SingletonTypeTreeTag: ClassTag[SingletonTypeTree]     = ClassTag[SingletonTypeTree](classOf[SingletonTypeTree])
  implicit val StarTag: ClassTag[Star]                               = ClassTag[Star](classOf[Star])
  implicit val SuperTag: ClassTag[Super]                             = ClassTag[Super](classOf[Super])
  implicit val SymTreeTag: ClassTag[SymTree]                         = ClassTag[SymTree](classOf[SymTree])
  implicit val TemplateTag: ClassTag[Template]                       = ClassTag[Template](classOf[Template])
  implicit val TermTreeTag: ClassTag[TermTree]                       = ClassTag[TermTree](classOf[TermTree])
  implicit val ThisTag: ClassTag[This]                               = ClassTag[This](classOf[This])
  implicit val ThrowTag: ClassTag[Throw]                             = ClassTag[Throw](classOf[Throw])
  implicit val TreeTag: ClassTag[Tree]                               = ClassTag[Tree](classOf[Tree])
  implicit val TryTag: ClassTag[Try]                                 = ClassTag[Try](classOf[Try])
  implicit val TypTreeTag: ClassTag[TypTree]                         = ClassTag[TypTree](classOf[TypTree])
  implicit val TypeApplyTag: ClassTag[TypeApply]                     = ClassTag[TypeApply](classOf[TypeApply])
  implicit val TypeBoundsTreeTag: ClassTag[TypeBoundsTree]           = ClassTag[TypeBoundsTree](classOf[TypeBoundsTree])
  implicit val TypeDefTag: ClassTag[TypeDef]                         = ClassTag[TypeDef](classOf[TypeDef])
  implicit val TypeTreeTag: ClassTag[TypeTree]                       = ClassTag[TypeTree](classOf[TypeTree])
  implicit val TypedTag: ClassTag[Typed]                             = ClassTag[Typed](classOf[Typed])
  implicit val UnApplyTag: ClassTag[UnApply]                         = ClassTag[UnApply](classOf[UnApply])
  implicit val ValDefTag: ClassTag[ValDef]                           = ClassTag[ValDef](classOf[ValDef])
  implicit val ValOrDefDefTag: ClassTag[ValOrDefDef]                 = ClassTag[ValOrDefDef](classOf[ValOrDefDef])
}

trait TreesStats {
  self: Statistics =>
  val symbolTable: SymbolTable
  val treeNodeCount = newView("#created tree nodes")(symbolTable.nodeCount)
  val retainedCount  = newCounter("#retained tree nodes")
  val retainedByType = newByClass("#retained tree nodes by type")(newCounter(""))
}
