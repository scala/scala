package scala.tools.nsc.ast;

import java.io.StringWriter;
import java.io.PrintWriter;
import scala.tools.util.Position;

class Trees: Global {

  abstract class Tree {

    var pos: int = Position.NOPOS;
    var tpe: Type = _;

    def setPos(p: int): Tree = { pos = p; this }
    def setType(tp: Type): Tree = { tpe = tp; this }

    def symbol: Symbol = null;
    def symbol_=(sym: Symbol): unit =
      throw new Error("symbol_= inapplicable for " + this);
    def setSymbol(sym: Symbol): Tree = { symbol = sym; this }

    def hasSymbol = false;
    def isDef = false;
    def isTerm = false;
    def isType = false;
    def isEmpty = false;

    override def toString(): String = {
      val buffer = new StringWriter();
      val printer = treePrinters.create(new PrintWriter(buffer));
      printer.print(this); printer.flush;
      buffer.toString()
    }

    def duplicate: Tree =
      new Transformer(new StrictTreeCopier) transform this;
  }

  abstract class SymTree extends Tree {
    override def hasSymbol = true;
    override var symbol: Symbol = NoSymbol;
  }

  abstract class DefTree extends SymTree {
    override def isDef = true;
  }

  abstract class TermTree extends Tree {
    override def isTerm = true;
  }

  abstract class TypeTree extends Tree {
    override def isType = true;
  }

  /** The empty tree */
  case object EmptyTree extends TermTree {
    tpe = NoType;
    override def isEmpty = true;
  }

  /** Class definition */
  case class ClassDef(mods: int, name: Name, tparams: List[AbsTypeDef], tp: Tree, impl: Template)
       extends DefTree;

  /** Package clause */
  case class PackageDef(name: Name, stats: List[Tree])
       extends DefTree;

  /** Singleton object definition */
  case class ModuleDef(mods: int, name: Name, tp: Tree, impl: Template)
       extends DefTree;

  /** Value definition */
  case class ValDef(mods: int, name: Name, tp: Tree, rhs: Tree)
       extends DefTree;

  /** Method definition */
  case class DefDef(mods: int, name: Name, tparams: List[AbsTypeDef],
		    vparams: List[List[ValDef]], tp: Tree, rhs: Tree)
       extends DefTree;

  /** Abstract type or type parameter */
  case class AbsTypeDef(mods: int, name: Name, lo: Tree, hi: Tree)
       extends DefTree;

  /** Type alias */
  case class AliasTypeDef(mods: int, name: Name, tparams: List[AbsTypeDef],
			  rhs: Tree)
       extends DefTree;

  /** Labelled expression - the symbols in the array (must be Idents!)
   *  are those the label takes as argument */
  case class LabelDef(name: Name, params: List[Ident], rhs: Tree)
       extends DefTree with TermTree;

  /** Import clause */
  case class Import(expr: Tree, selectors: List[Pair[Name, Name]])
       extends SymTree;

  /** Pattern definition, eliminated by Analyzer */
  case class PatDef(mods: int, pat: Tree, rhs: Tree)
       extends Tree;

  /** Attribuetd definition */
  case class Attributed(attribute: Tree, definition: Tree)
       extends Tree;

  /** Documented definition, eliminated by analyzer */
  case class DocDef(comment: String, definition: Tree)
       extends Tree;

  /** Instantiation template */
  case class Template(parents: List[Tree], body: List[Tree])
       extends SymTree;

  /** Block of expressions (semicolon separated expressions) */
  case class Block(stats: List[Tree], expr: Tree)
       extends TermTree;

  /** Visitor (a sequence of cases), eliminated by TransMatch */
  case class Visitor(cases: List[CaseDef])
       extends TermTree;

  /** Case clause in a pattern match, eliminated by TransMatch */
  case class CaseDef(pat: Tree, guard: Tree, body: Tree)
       extends Tree;

  /** Sequence of expression/patterns (comma separated expressions),
   *  eliminated by TransMatch */
  case class Sequence(trees: List[Tree])
       extends TermTree;

  /** Alternatives of patterns, eliminated by TransMatch */
  case class Alternative(trees: List[Tree])
       extends TermTree;

  /** Bind of a variable to a rhs pattern, eliminated by TransMatch */
  case class Bind(name: Name, rhs: Tree)
       extends TermTree;

  /** Anonymous function, eliminated by analyzer */
  case class Function(vparams: List[ValDef], body: Tree)
       extends TermTree;

  /** Assignment */
  case class Assign(lhs: Tree, rhs: Tree)
       extends TermTree;

  /** Conditional expression */
  case class If(cond: Tree, thenp: Tree, elsep: Tree)
       extends TermTree;

  /** For comprehension */
  case class For(enumerators: List[Tree], body: Tree, isForYield: boolean)
       extends TermTree;

  /** Switch, introduced by refcheck */
  case class Switch(test: Tree, tags: List[int], bodies: List[Tree],
		    default: Tree)
       extends TermTree;

  /** Return expression */
  case class Return(expr: Tree)
       extends TermTree;

  case class Try(block: Tree, catcher: Tree, finalizer: Tree)
       extends TermTree;

  /** Throw expression */
  case class Throw(expr: Tree)
       extends TermTree;

  /** Object instantiation
   *   @param init   either a constructor or a template
   */
  case class New(init: Tree)
       extends TermTree;

  /** Type annotation, eliminated by explicit outer */
  case class Typed(expr: Tree, tp: Tree)
       extends TermTree;

  /** Type application */
  case class TypeApply(fun: Tree, args: List[Tree])
       extends TermTree;

  /** Value application */
  case class Apply(fun: Tree, args: List[Tree])
       extends TermTree;

  /** Super reference */
  case class Super(qual: Name, mixin: Name)
       extends TermTree with SymTree;

  /** Self reference */
  case class This(qual: Name)
        extends TermTree with SymTree;

  /** Designator */
  case class Select(qualifier: Tree, selector: Name)
       extends SymTree {
    override def isTerm = selector.isTermName;
    override def isType = selector.isTypeName;
  }

  /** Identifier */
  case class Ident(name: Name)
       extends SymTree {
    override def isTerm = name.isTermName;
    override def isType = name.isTypeName;
  }

  /** Literal */
  case class Literal(value: Any)
       extends TermTree;

  /** General type term, introduced by RefCheck. */
  case class EmptyTypeTree() extends TypeTree {
    override def isEmpty = true;
  }

  /** Singleton type, eliminated by RefCheck */
  case class SingletonTypeTree(ref: Tree)
        extends TypeTree;

  /** Type selection, eliminated by RefCheck */
  case class SelectFromTypeTree(qualifier: Tree, selector: Name)
       extends TypeTree with SymTree;

  /** Function type, eliminated by RefCheck */
  case class FunctionTypeTree(argtpes: List[Tree], restpe: Tree)
       extends TypeTree;

  /** Intersection type, eliminated by RefCheck */
  case class IntersectionTypeTree(parents: List[Tree])
       extends TypeTree;

  /** Refinement type, eliminated by RefCheck */
  case class RefinementTypeTree(base: Tree, members: List[Tree])
       extends TypeTree;

  /** Applied type, eliminated by RefCheck */
  case class AppliedTypeTree(tp: Tree, args: List[Tree])
       extends TypeTree;

/* A standard pattern match
  case EmptyTree =>
  case ClassDef(mods, name, tparams, tp, impl) =>
  case PackageDef(name, stats) =>
  case ModuleDef(mods, name, tp, impl) =>
  case ValDef(mods, name, tp, rhs) =>
  case DefDef(mods, name, tparams, vparams, tp, rhs) =>
  case AbsTypeDef(mods, name, lo, hi) =>
  case AliasTypeDef(mods, name, tparams, rhs) =>
  case LabelDef(name, params, rhs) =>
  case Import(expr, selectors) =>
  case PatDef(mods, pat, rhs) =>
  case Attributed(attribute, definition) =>
  case DocDef(comment, definition) =>
  case Template(parents, body) =>
  case Block(stats, expr) =>
  case Visitor(cases) =>
  case CaseDef(pat, guard, body) =>
  case Sequence(trees) =>
  case Alternative(trees) =>
  case Bind(name, rhs) =>
  case Function(vparams, body) =>
  case Assign(lhs, rhs) =>
  case For(enumerators, body, isForYield) =>
  case If(cond, thenp, elsep) =>
  case Switch(test, tags, bodies, default) =>
  case Return(expr) =>
  case Try(block, catcher, finalizer) =>
  case Throw(expr) =>
  case New(init) =>
  case Typed(expr, tp) =>
  case TypeApply(fun, args) =>
  case Apply(fun, args) =>
  case Super(qual, mixin) =>
  case This(qual) =>
  case Select(qualifier, selector) =>
  case Ident(name) =>
  case Literal(value) =>
  case EmptyTypeTree() =>
  case SingletonTypeTree(ref) =>
  case SelectFromTypeTree(qualifier, selector) =>
  case FunctionTypeTree(argtpes, restpe) =>
  case IntersectionTypeTree(parents) =>
  case RefinementTypeTree(base, members) =>
  case AppliedTypeTree(tp, args) =>
*/

  trait TreeCopier {
    def ClassDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], tp: Tree, impl: Template): ClassDef;
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]): PackageDef;
    def ModuleDef(tree: Tree, mods: int, name: Name, tp: Tree, impl: Template): ModuleDef;
    def ValDef(tree: Tree, mods: int, name: Name, tp: Tree, rhs: Tree): ValDef;
    def DefDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], vparams: List[List[ValDef]], tp: Tree, rhs: Tree): DefDef;
    def AbsTypeDef(tree: Tree, mods: int, name: Name, lo: Tree, hi: Tree): AbsTypeDef;
    def AliasTypeDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], rhs: Tree): AliasTypeDef;
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree): LabelDef;
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]): Import;
    def PatDef(tree: Tree, mods: int, pat: Tree, rhs: Tree): PatDef;
    def Attributed(tree: Tree, attribute: Tree, definition: Tree): Attributed;
    def DocDef(tree: Tree, comment: String, definition: Tree): DocDef;
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]): Template;
    def Block(tree: Tree, stats: List[Tree], expr: Tree): Block;
    def Visitor(tree: Tree, cases: List[CaseDef]): Visitor;
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree): CaseDef;
    def Sequence(tree: Tree, trees: List[Tree]): Sequence;
    def Alternative(tree: Tree, trees: List[Tree]): Alternative;
    def Bind(tree: Tree, name: Name, rhs: Tree): Bind;
    def Function(tree: Tree, vparams: List[ValDef], body: Tree): Function;
    def Assign(tree: Tree, lhs: Tree, rhs: Tree): Assign;
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree): If;
    def For(tree: Tree, enumerators: List[Tree], body: Tree, isForYield: boolean): For;
    def Switch(tree: Tree, test: Tree, tags: List[int], bodies: List[Tree], default: Tree): Switch;
    def Return(tree: Tree, expr: Tree): Return;
    def Try(tree: Tree, block: Tree, catcher: Tree, finalizer: Tree): Try;
    def Throw(tree: Tree, expr: Tree): Throw;
    def New(tree: Tree, init: Tree): New;
    def Typed(tree: Tree, expr: Tree, tp: Tree): Typed;
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]): TypeApply;
    def Apply(tree: Tree, fun: Tree, args: List[Tree]): Apply;
    def Super(tree: Tree, qual: Name, mixin: Name): Super;
    def This(tree: Tree, qual: Name): This;
    def Select(tree: Tree, qualifier: Tree, selector: Name): Select;
    def Ident(tree: Tree, name: Name): Ident;
    def Literal(tree: Tree, value: Any): Literal;
    def EmptyTypeTree(tree: Tree): EmptyTypeTree;
    def SingletonTypeTree(tree: Tree, ref: Tree): SingletonTypeTree;
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name): SelectFromTypeTree;
    def FunctionTypeTree(tree: Tree, argtpes: List[Tree], restpe: Tree): FunctionTypeTree;
    def IntersectionTypeTree(tree: Tree, parents: List[Tree]): IntersectionTypeTree;
    def RefinementTypeTree(tree: Tree, base: Tree, members: List[Tree]): RefinementTypeTree;
    def AppliedTypeTree(tree: Tree, tp: Tree, args: List[Tree]): AppliedTypeTree;
  }

  class StrictTreeCopier extends TreeCopier {
    def ClassDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], tp: Tree, impl: Template) =
      { val t = new ClassDef(mods, name, tparams, tp, impl); t.setPos(tree.pos); t }
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]) =
      { val t = new PackageDef(name, stats); t.setPos(tree.pos); t }
    def ModuleDef(tree: Tree, mods: int, name: Name, tp: Tree, impl: Template) =
      { val t = new ModuleDef(mods, name, tp, impl); t.setPos(tree.pos); t }
    def ValDef(tree: Tree, mods: int, name: Name, tp: Tree, rhs: Tree) =
      { val t = new ValDef(mods, name, tp, rhs); t.setPos(tree.pos); t }
    def DefDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], vparams: List[List[ValDef]], tp: Tree, rhs: Tree) =
      { val t = new DefDef(mods, name, tparams, vparams, tp, rhs); t.setPos(tree.pos); t }
    def AbsTypeDef(tree: Tree, mods: int, name: Name, lo: Tree, hi: Tree) =
      { val t = new AbsTypeDef(mods, name, lo, hi); t.setPos(tree.pos); t }
    def AliasTypeDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], rhs: Tree) =
      { val t = new AliasTypeDef(mods, name, tparams, rhs); t.setPos(tree.pos); t }
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) =
      { val t = new LabelDef(name, params, rhs); t.setPos(tree.pos); t }
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]) =
      { val t = new Import(expr, selectors); t.setPos(tree.pos); t }
    def PatDef(tree: Tree, mods: int, pat: Tree, rhs: Tree) =
      { val t = new PatDef(mods, pat, rhs); t.setPos(tree.pos); t }
    def Attributed(tree: Tree, attribute: Tree, definition: Tree) =
      { val t = new Attributed(attribute, definition); t.setPos(tree.pos); t }
    def DocDef(tree: Tree, comment: String, definition: Tree) =
      { val t = new DocDef(comment, definition); t.setPos(tree.pos); t }
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]) =
      { val t = new Template(parents, body); t.setPos(tree.pos); t }
    def Block(tree: Tree, stats: List[Tree], expr: Tree) =
      { val t = new Block(stats, expr); t.setPos(tree.pos); t }
    def Visitor(tree: Tree, cases: List[CaseDef]) =
      { val t = new Visitor(cases); t.setPos(tree.pos); t }
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) =
      { val t = new CaseDef(pat, guard, body); t.setPos(tree.pos); t }
    def Sequence(tree: Tree, trees: List[Tree]) =
      { val t = new Sequence(trees); t.setPos(tree.pos); t }
    def Alternative(tree: Tree, trees: List[Tree]) =
      { val t = new Alternative(trees); t.setPos(tree.pos); t }
    def Bind(tree: Tree, name: Name, rhs: Tree) =
      { val t = new Bind(name, rhs); t.setPos(tree.pos); t }
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) =
      { val t = new Function(vparams, body); t.setPos(tree.pos); t }
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) =
      { val t = new Assign(lhs, rhs); t.setPos(tree.pos); t }
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) =
      { val t = new If(cond, thenp, elsep); t.setPos(tree.pos); t }
    def For(tree: Tree, enumerators: List[Tree], body: Tree, isForYield: boolean) =
      { val t = new For(enumerators, body, isForYield); t.setPos(tree.pos); t }
    def Switch(tree: Tree, test: Tree, tags: List[int], bodies: List[Tree], default: Tree) =
      { val t = new Switch(test, tags, bodies, default); t.setPos(tree.pos); t }
    def Return(tree: Tree, expr: Tree) =
      { val t = new Return(expr); t.setPos(tree.pos); t }
    def Try(tree: Tree, block: Tree, catcher: Tree, finalizer: Tree) =
      { val t = new Try(block, catcher, finalizer); t.setPos(tree.pos); t }
    def Throw(tree: Tree, expr: Tree) =
      { val t = new Throw(expr); t.setPos(tree.pos); t }
    def New(tree: Tree, init: Tree) =
      { val t = new New(init); t.setPos(tree.pos); t }
    def Typed(tree: Tree, expr: Tree, tp: Tree) =
      { val t = new Typed(expr, tp); t.setPos(tree.pos); t }
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) =
      { val t = new TypeApply(fun, args); t.setPos(tree.pos); t }
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) =
      { val t = new Apply(fun, args); t.setPos(tree.pos); t }
    def Super(tree: Tree, qual: Name, mixin: Name) =
      { val t = new Super(qual, mixin); t.setPos(tree.pos); t }
    def This(tree: Tree, qual: Name) =
      { val t = new This(qual); t.setPos(tree.pos); t }
    def Select(tree: Tree, qualifier: Tree, selector: Name) =
      { val t = new Select(qualifier, selector); t.setPos(tree.pos); t }
    def Ident(tree: Tree, name: Name) =
      { val t = new Ident(name); t.setPos(tree.pos); t }
    def Literal(tree: Tree, value: Any) =
      { val t = new Literal(value); t.setPos(tree.pos); t }
    def EmptyTypeTree(tree: Tree) =
      { val t = new EmptyTypeTree(); t.setPos(tree.pos); t }
    def SingletonTypeTree(tree: Tree, ref: Tree) =
      { val t = new SingletonTypeTree(ref); t.setPos(tree.pos); t }
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) =
      { val t = new SelectFromTypeTree(qualifier, selector); t.setPos(tree.pos); t }
    def FunctionTypeTree(tree: Tree, argtpes: List[Tree], restpe: Tree) =
      { val t = new FunctionTypeTree(argtpes, restpe); t.setPos(tree.pos); t }
    def IntersectionTypeTree(tree: Tree, parents: List[Tree]) =
      { val t = new IntersectionTypeTree(parents); t.setPos(tree.pos); t }
    def RefinementTypeTree(tree: Tree, base: Tree, members: List[Tree]) =
      { val t = new RefinementTypeTree(base, members); t.setPos(tree.pos); t }
    def AppliedTypeTree(tree: Tree, tp: Tree, args: List[Tree]) =
      { val t = new AppliedTypeTree(tp, args); t.setPos(tree.pos); t }
  }

  class LazyTreeCopier(copy: TreeCopier) extends TreeCopier {
    def this() = this(new StrictTreeCopier);
    def ClassDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], tp: Tree, impl: Template) = tree match {
      case t @ ClassDef(mods0, name0, tparams0, tp0, impl0)
      if (mods0 == mods && name0 == name && tparams0 == tparams && tp0 == tp && impl0 == impl) => t
      case _ => copy.ClassDef(tree, mods, name, tparams, tp, impl)
    }
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]) = tree match {
      case t @ PackageDef(name0, stats0)
      if (name0 == name && stats0 == stats) => t
      case _ => copy.PackageDef(tree, name, stats)
    }
    def ModuleDef(tree: Tree, mods: int, name: Name, tp: Tree, impl: Template) = tree match {
      case t @ ModuleDef(mods0, name0, tp0, impl0)
      if (mods0 == mods && name0 == name && tp0 == tp && impl0 == impl) => t
      case _ => copy.ModuleDef(tree, mods, name, tp, impl)
    }
    def ValDef(tree: Tree, mods: int, name: Name, tp: Tree, rhs: Tree) = tree match {
      case t @ ValDef(mods0, name0, tp0, rhs0)
      if (mods0 == mods && name0 == name && tp0 == tp && rhs0 == rhs) => t
      case _ => copy.ValDef(tree, mods, name, tp, rhs)
    }
    def DefDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], vparams: List[List[ValDef]], tp: Tree, rhs: Tree) = tree match {
      case t @ DefDef(mods0, name0, tparams0, vparams0, tp0, rhs0)
      if (mods0 == mods && name0 == name && tparams0 == tparams && vparams0 == vparams && tp0 == tp && rhs == rhs0) => t
      case _ => copy.DefDef(tree, mods, name, tparams, vparams, tp, rhs)
    }
    def AbsTypeDef(tree: Tree, mods: int, name: Name, lo: Tree, hi: Tree) = tree match {
      case t @ AbsTypeDef(mods0, name0, lo0, hi0)
      if (mods0 == mods && name0 == name && lo0 == lo && hi0 == hi) => t
      case _ => copy.AbsTypeDef(tree, mods, name, lo, hi)
    }
    def AliasTypeDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], rhs: Tree) = tree match {
      case t @ AliasTypeDef(mods0, name0, tparams0, rhs0)
      if (mods0 == mods && name0 == name && tparams0 == tparams && rhs0 == rhs) => t
      case _ => copy.AliasTypeDef(tree, mods, name, tparams, rhs)
    }
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) = tree match {
      case t @ LabelDef(name0, params0, rhs0)
      if (name0 == name && params0 == params && rhs0 == rhs) => t
      case _ => copy.LabelDef(tree, name, params, rhs)
    }
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]) = tree match {
      case t @ Import(expr0, selectors0)
      if (expr0 == expr && selectors0 == selectors) => t
      case _ => copy.Import(tree, expr, selectors)
    }
    def PatDef(tree: Tree, mods: int, pat: Tree, rhs: Tree) = tree match {
      case t @ PatDef(mods0, pat0, rhs0)
      if (mods0 == mods && pat0 == pat && rhs0 == rhs) => t
      case _ => copy.PatDef(tree, mods, pat, rhs)
    }
    def Attributed(tree: Tree, attribute: Tree, definition: Tree) = tree match {
      case t @ Attributed(attribute0, definition0)
      if (attribute0 == attribute && definition0 == definition) => t
      case _ => copy.Attributed(tree, attribute, definition)
    }
    def DocDef(tree: Tree, comment: String, definition: Tree) = tree match {
      case t @ DocDef(comment0, definition0)
      if (comment0 == comment && definition0 == definition) => t
      case _ => copy.DocDef(tree, comment, definition)
    }
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]) = tree match {
      case t @ Template(parents0, body0)
      if (parents0 == parents && body0 == body) => t
      case _ => copy.Template(tree, parents, body)
    }
    def Block(tree: Tree, stats: List[Tree], expr: Tree) = tree match {
      case t @ Block(stats0, expr0)
      if (stats0 == stats && expr0 == expr) => t
      case _ => copy.Block(tree, stats, expr)
    }
    def Visitor(tree: Tree, cases: List[CaseDef]) = tree match {
      case t @ Visitor(cases0)
      if (cases == cases0) => t
      case _ => copy.Visitor(tree, cases)
    }
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) = tree match {
      case t @ CaseDef(pat0, guard0, body0)
      if (pat0 == pat && guard0 == guard && body0 == body) => t
      case _ => copy.CaseDef(tree, pat, guard, body)
    }
    def Sequence(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Sequence(trees0)
      if (trees0 == trees) => t
      case _ => copy.Sequence(tree, trees)
    }
    def Alternative(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Alternative(trees0)
      if (trees0 == trees) => t
      case _ => copy.Alternative(tree, trees)
    }
    def Bind(tree: Tree, name: Name, rhs: Tree) = tree match {
      case t @ Bind(name0, rhs0)
      if (name0 == name && rhs0 == rhs) => t
      case _ => copy.Bind(tree, name, rhs)
    }
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) = tree match {
      case t @ Function(vparams0, body0)
      if (vparams0 == vparams && body0 == body) => t
      case _ => copy.Function(tree, vparams, body)
    }
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ Assign(lhs0, rhs0)
      if (lhs0 == lhs && rhs0 == rhs) => t
      case _ => copy.Assign(tree, lhs, rhs)
    }
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) = tree match {
      case t @ If(cond0, thenp0, elsep0)
      if (cond0 == cond && thenp0 == thenp && elsep0 == elsep) => t
      case _ => copy.If(tree, cond, thenp, elsep)
    }
    def For(tree: Tree, enumerators: List[Tree], body: Tree, isForYield: boolean) = tree match {
      case t @ For(enumerators0, body0, isForYield0)
      if (enumerators0 == enumerators && body0 == body && isForYield0 == isForYield) => t
      case _ => copy.For(tree, enumerators, body, isForYield)
    }
    def Switch(tree: Tree, test: Tree, tags: List[int], bodies: List[Tree], default: Tree) = tree match {
      case t @ Switch(test0, tags0, bodies0, default0)
      if (test0 == test && tags0 == tags && bodies0 == bodies && default0 == default) => t
      case _ => copy.Switch(tree, test, tags, bodies, default)
    }
    def Return(tree: Tree, expr: Tree) = tree match {
      case t @ Return(expr0)
      if (expr0 == expr) => t
      case _ => copy.Return(tree, expr)
    }
    def Try(tree: Tree, block: Tree, catcher: Tree, finalizer: Tree) = tree match {
      case t @ Try(block0, catcher0, finalizer0)
      if (block0 == block && catcher0 == catcher && finalizer0 == finalizer) => t
      case _ => copy.Try(tree, block, catcher, finalizer)
    }
    def Throw(tree: Tree, expr: Tree) = tree match {
      case t @ Throw(expr0)
      if (expr0 == expr) => t
      case _ => copy.Throw(tree, expr)
    }
    def New(tree: Tree, init: Tree) = tree match {
      case t @ New(init0)
      if (init0 == init) => t
      case _ => copy.New(tree, init)
    }
    def Typed(tree: Tree, expr: Tree, tp: Tree) = tree match {
      case t @ Typed(expr0, tp0)
      if (expr0 == expr && tp0 == tp) => t
      case _ => copy.Typed(tree, expr, tp)
    }
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ TypeApply(fun0, args0)
      if (fun0 == fun && args0 == args) => t
      case _ => copy.TypeApply(tree, fun, args)
    }
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ Apply(fun0, args0)
      if (fun0 == fun && args0 == args) => t
      case _ => copy.Apply(tree, fun, args)
    }
    def Super(tree: Tree, qual: Name, mixin: Name) = tree match {
      case t @ Super(qual0, mixin0)
      if (qual0 == qual && mixin0 == mixin) => t
      case _ => copy.Super(tree, qual, mixin)
    }
    def This(tree: Tree, qual: Name) = tree match {
      case t @ This(qual0)
      if (qual0 == qual) => t
      case _ => copy.This(tree, qual)
    }
    def Select(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ Select(qualifier0, selector0)
      if (qualifier0 == qualifier && selector0 == selector) => t
      case _ => copy.Select(tree, qualifier, selector)
    }
    def Ident(tree: Tree, name: Name) = tree match {
      case t @ Ident(name0)
      if (name0 == name) => t
      case _ => copy.Ident(tree, name)
    }
    def Literal(tree: Tree, value: Any) = tree match {
      case t @ Literal(value0)
      if (value0 == value) => t
      case _ => copy.Literal(tree, value)
    }
    def EmptyTypeTree(tree: Tree) = tree match {
      case t @ EmptyTypeTree() => t
      case _ => copy.EmptyTypeTree(tree)
    }
    def SingletonTypeTree(tree: Tree, ref: Tree) = tree match {
      case t @ SingletonTypeTree(ref0)
      if (ref0 == ref) => t
      case _ => copy.SingletonTypeTree(tree, ref)
    }
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ SelectFromTypeTree(qualifier0, selector0)
      if (qualifier0 == qualifier && selector0 == selector) => t
      case _ => copy.SelectFromTypeTree(tree, qualifier, selector)
    }
    def FunctionTypeTree(tree: Tree, argtpes: List[Tree], restpe: Tree) = tree match {
      case t @ FunctionTypeTree(argtpes0, restpe0)
      if (argtpes0 == argtpes && restpe0 == restpe) => t
      case _ => copy.FunctionTypeTree(tree, argtpes, restpe)
    }
    def IntersectionTypeTree(tree: Tree, parents: List[Tree]) = tree match {
      case t @ IntersectionTypeTree(parents0)
      if (parents0 == parents) => t
      case _ => copy.IntersectionTypeTree(tree, parents)
    }
    def RefinementTypeTree(tree: Tree, base: Tree, members: List[Tree]) = tree match {
      case t @ RefinementTypeTree(base0, members0)
      if (base0 == base && members0 == members) => t
      case _ => copy.RefinementTypeTree(tree, base, members)
    }
    def AppliedTypeTree(tree: Tree, tp: Tree, args: List[Tree]) = tree match {
      case t @ AppliedTypeTree(tp0, args0)
      if (tp0 == tp && args0 == args) => t
      case _ => copy.AppliedTypeTree(tree, tp, args)
    }
  }

  class Transformer(val copy: TreeCopier) {
    def this() = this(new LazyTreeCopier);
    def transform(tree: Tree): Tree = tree match {
      case EmptyTree =>
        tree
      case ClassDef(mods, name, tparams, tp, impl) =>
        copy.ClassDef(tree, mods, name, transformAbsTypeDefs(tparams), transform(tp), transformTemplate(impl))
      case PackageDef(name, stats) =>
        copy.PackageDef(tree, name, transformTrees(stats))
      case ModuleDef(mods, name, tp, impl) =>
        copy.ModuleDef(tree, mods, name, transform(tp), transformTemplate(impl))
      case ValDef(mods, name, tp, rhs) =>
        copy.ValDef(tree, mods, name, transform(tp), transform(rhs))
      case DefDef(mods, name, tparams, vparams, tp, rhs) =>
        copy.DefDef(tree, mods, name, transformAbsTypeDefs(tparams), transformValDefss(vparams), transform(tp), transform(rhs))
      case AbsTypeDef(mods, name, lo, hi) =>
        copy.AbsTypeDef(tree, mods, name, transform(lo), transform(hi))
      case AliasTypeDef(mods, name, tparams, rhs) =>
        copy.AliasTypeDef(tree, mods, name, transformAbsTypeDefs(tparams), transform(rhs))
      case LabelDef(name, params, rhs) =>
        copy.LabelDef(tree, name, transformIdents(params), transform(rhs))
      case Import(expr, selectors) =>
        copy.Import(tree, transform(expr), selectors)
      case PatDef(mods, pat, rhs) =>
        copy.PatDef(tree, mods, transform(pat), transform(rhs))
      case Attributed(attribute, definition) =>
        copy.Attributed(tree, transform(attribute), transform(definition))
      case DocDef(comment, definition) =>
        copy.DocDef(tree, comment, transform(definition))
      case Template(parents, body) =>
        copy.Template(tree, transformTrees(parents), transformTrees(body))
      case Block(stats, expr) =>
        copy.Block(tree, transformTrees(stats), transform(expr))
      case Visitor(cases) =>
        copy.Visitor(tree, transformCaseDefs(cases))
      case CaseDef(pat, guard, body) =>
        copy.CaseDef(tree, transform(pat), transform(guard), transform(body))
      case Sequence(trees) =>
        copy.Sequence(tree, transformTrees(trees))
      case Alternative(trees) =>
        copy.Alternative(tree, transformTrees(trees))
      case Bind(name, rhs) =>
        copy.Bind(tree, name, transform(rhs))
      case Function(vparams, body) =>
        copy.Function(tree, transformValDefs(vparams), transform(body))
      case Assign(lhs, rhs) =>
        copy.Assign(tree, transform(lhs), transform(rhs))
      case For(enumerators, body: Tree, isForYield) =>
        copy.For(tree, transformTrees(enumerators), transform(body), isForYield)
      case If(cond, thenp, elsep) =>
        copy.If(tree, transform(cond), transform(thenp), transform(elsep))
      case Switch(test, tags, bodies, default) =>
	copy.Switch(tree, transform(test), tags, transformTrees(bodies), transform(default))
      case Return(expr) =>
	copy.Return(tree, transform(expr))
      case Try(block, catcher, finalizer) =>
        copy.Try(tree, transform(block), transform(catcher), transform(finalizer))
      case Throw(expr) =>
        copy.Throw(tree, transform(expr))
      case New(init) =>
        copy.New(tree, transform(init))
      case Typed(expr, tp) =>
        copy.Typed(tree, transform(expr), transform(tp))
      case TypeApply(fun, args) =>
        copy.TypeApply(tree, transform(fun), transformTrees(args))
      case Apply(fun, args) =>
        copy.Apply(tree, transform(fun), transformTrees(args))
      case Super(qual, mixin) =>
        copy.Super(tree, qual, mixin)
      case This(qual) =>
        copy.This(tree, qual)
      case Select(qualifier, selector) =>
        copy.Select(tree, transform(qualifier), selector)
      case Ident(name) =>
        copy.Ident(tree, name)
      case Literal(value) =>
        copy.Literal(tree, value)
      case EmptyTypeTree() =>
        copy.EmptyTypeTree(tree)
      case SingletonTypeTree(ref) =>
        copy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        copy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case FunctionTypeTree(argtpes, restpe) =>
        copy.FunctionTypeTree(tree, transformTrees(argtpes), transform(restpe))
      case IntersectionTypeTree(parents) =>
        copy.IntersectionTypeTree(tree, transformTrees(parents))
      case RefinementTypeTree(base, members) =>
        copy.RefinementTypeTree(tree, transform(base), transformTrees(members))
      case AppliedTypeTree(tp, args) =>
        copy.AppliedTypeTree(tree, transform(tp), transformTrees(args))
    }

    def transformTrees(trees: List[Tree]): List[Tree] =
      List.transform(trees)(tree => transform(tree));
    def transformTemplate(tree: Template): Template =
      transform(tree: Tree).asInstanceOf[Template];
    def transformAbsTypeDefs(trees: List[AbsTypeDef]): List[AbsTypeDef] =
      List.transform(trees)(tree => transform(tree).asInstanceOf[AbsTypeDef]);
    def transformValDefs(trees: List[ValDef]): List[ValDef] =
      List.transform(trees)(tree => transform(tree).asInstanceOf[ValDef]);
    def transformValDefss(treess: List[List[ValDef]]): List[List[ValDef]] =
      List.transform(treess)(tree => transformValDefs(tree));
    def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
      List.transform(trees)(tree => transform(tree).asInstanceOf[CaseDef]);
    def transformIdents(trees: List[Ident]): List[Ident] =
      List.transform(trees)(tree => transform(tree).asInstanceOf[Ident]);
  }

  class Traverser {
    def traverse(tree: Tree): unit = tree match {
      case ClassDef(mods, name, tparams, tp, impl) =>
        traverseTrees(tparams); traverse(tp); traverse(impl)
      case PackageDef(name, stats) =>
        traverseTrees(stats)
      case ModuleDef(mods, name, tp, impl) =>
        traverse(tp); traverse(impl)
      case ValDef(mods, name, tp, rhs) =>
        traverse(tp); traverse(rhs)
      case DefDef(mods, name, tparams, vparams, tp, rhs) =>
        traverseTrees(tparams); traverseTreess(vparams); traverse(tp); traverse(rhs)
      case AbsTypeDef(mods, name, lo, hi) =>
        traverse(lo); traverse(hi);
      case AliasTypeDef(mods, name, tparams, rhs) =>
        traverseTrees(tparams); traverse(rhs)
      case LabelDef(name, params, rhs) =>
        traverseTrees(params); traverse(rhs)
      case Import(expr, selectors) =>
        traverse(expr)
      case PatDef(mods, pat, rhs) =>
        traverse(pat); traverse(rhs)
      case Attributed(attribute, definition) =>
        traverse(attribute); traverse(definition)
      case DocDef(comment, definition) =>
        traverse(definition)
      case Template(parents, body) =>
        traverseTrees(parents); traverseTrees(body)
      case Block(stats, expr) =>
        traverseTrees(stats); traverse(expr)
      case Visitor(cases) =>
        traverseTrees(cases)
      case CaseDef(pat, guard, body) =>
        traverse(pat); traverse(guard); traverse(body)
      case Sequence(trees) =>
        traverseTrees(trees)
      case Alternative(trees) =>
        traverseTrees(trees)
      case Bind(name, rhs) =>
        traverse(rhs)
      case Function(vparams, body) =>
        traverseTrees(vparams); traverse(body)
      case Assign(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case For(enumerators, body: Tree, isForYield) =>
        traverseTrees(enumerators); traverse(body)
      case If(cond, thenp, elsep) =>
        traverse(cond); traverse(thenp); traverse(elsep)
      case Switch(test, tags, bodies, default) =>
	traverse(test); traverseTrees(bodies); traverse(default)
      case Return(expr) =>
	traverse(expr)
      case Try(block, catcher, finalizer) =>
        traverse(block); traverse(catcher); traverse(finalizer)
      case Throw(expr) =>
        traverse(expr)
      case New(init) =>
        traverse(init)
      case Typed(expr, tp) =>
        traverse(expr); traverse(tp)
      case TypeApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case Apply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case Select(qualifier, selector) =>
        traverse(qualifier)
      case SingletonTypeTree(ref) =>
        traverse(ref)
      case SelectFromTypeTree(qualifier, selector) =>
        traverse(qualifier)
      case FunctionTypeTree(argtpes, restpe) =>
        traverseTrees(argtpes); traverse(restpe)
      case IntersectionTypeTree(parents) =>
        traverseTrees(parents)
      case RefinementTypeTree(base, members) =>
        traverse(base); traverseTrees(members)
      case AppliedTypeTree(tp, args) =>
        traverse(tp); traverseTrees(args)
      case EmptyTree | Super(_, _) | This(_) | Ident(_) | Literal(_) | EmptyTypeTree() =>
	{}
    }

    def traverseTrees(trees: List[Tree]): unit =
      trees foreach traverse;
    def traverseTreess(treess: List[List[Tree]]): unit =
      treess foreach traverseTrees;
  }

  final class TreeList {
    private var trees = List[Tree]();
    def append(t: Tree): TreeList = { trees = t :: trees; this }
    def append(ts: List[Tree]): TreeList = { trees = ts reverse_::: trees; this }
    def toList: List[Tree] = trees.reverse;
  }

  object posAssigner extends Traverser {
    private var pos: int = _;
    override def traverse(t: Tree): unit =
      if (t != EmptyTree && t.pos == Position.NOPOS) {
	t.pos = pos;
	super.traverse(t);
      }
    def atPos[T <: Tree](pos: int)(tree: T): T = {
      this.pos = pos;
      traverse(tree);
      tree
    }
  }
}

