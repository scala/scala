/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast;

import java.io.StringWriter;
import java.io.PrintWriter;
import scala.tools.util.Position;

class Trees: Global {

  abstract class Tree {

    var pos: int = Position.NOPOS;
    var tpe: Type = _;

    def setPos(p: int): this.type = { pos = p; this }
    def setType(tp: Type): this.type = { tpe = tp; this }

    def symbol: Symbol = null;
    def symbol_=(sym: Symbol): unit =
      throw new Error("symbol_= inapplicable for " + this);
    def setSymbol(sym: Symbol): this.type = { symbol = sym; this }

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

    override def equals(that: Any): boolean = that match {
      case t: Tree => this eq t
      case _ => false
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

  abstract class TypTree extends Tree {
    override def isType = true;
  }

  /** The empty tree */
  case object EmptyTree extends TermTree {
    tpe = NoType;
    override def isEmpty = true;
  }

  /** Package clause */
  case class PackageDef(name: Name, stats: List[Tree])
       extends DefTree;

  /** Class definition */
  case class ClassDef(mods: int, name: Name, tparams: List[AbsTypeDef], tpt: Tree, impl: Template)
       extends DefTree;

  /** Singleton object definition */
  case class ModuleDef(mods: int, name: Name, impl: Template)
       extends DefTree;

  /** Value definition */
  case class ValDef(mods: int, name: Name, tpt: Tree, rhs: Tree)
       extends DefTree;

  /** Method definition */
  case class DefDef(mods: int, name: Name, tparams: List[AbsTypeDef],
		    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)
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
  case class Bind(name: Name, body: Tree)
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

  /** Pattern matching expression */
  case class Match(selector: Tree, cases: List[CaseDef]) extends TermTree;

  /** Return expression */
  case class Return(expr: Tree)
       extends SymTree;

  case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)
       extends TermTree;

  /** Throw expression */
  case class Throw(expr: Tree)
       extends TermTree;

  /** Object instantiation
   *   @param init   either a constructor or a template
   */
  case class New(typeOrTempl: Tree)
       extends TermTree;

  /** Type annotation, eliminated by explicit outer */
  case class Typed(expr: Tree, tpt: Tree)
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
  case class TypeTree() extends TypTree {
    override def isEmpty = tpe == null;
  }

  /** Singleton type, eliminated by RefCheck */
  case class SingletonTypeTree(ref: Tree)
        extends TypTree;

  /** Type selection, eliminated by RefCheck */
  case class SelectFromTypeTree(qualifier: Tree, selector: Name)
       extends TypTree with SymTree;

  /** Intersection type, eliminated by RefCheck */
  case class CompoundTypeTree(parents: List[Tree], decls: List[Tree])
       extends TypTree;

  /** Applied type, eliminated by RefCheck */
  case class AppliedTypeTree(tpt: Tree, args: List[Tree])
       extends TypTree;

/* A standard pattern match
  case EmptyTree =>
  case PackageDef(name, stats) =>
  case ClassDef(mods, name, tparams, tpt, impl) =>
  case ModuleDef(mods, name, impl) =>                         (eliminated by refcheck)
  case ValDef(mods, name, tpt, rhs) =>
  case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
  case AbsTypeDef(mods, name, lo, hi) =>                          (eliminated by erasure)
  case AliasTypeDef(mods, name, tparams, rhs) =>                  (eliminated by erasure)
  case LabelDef(name, params, rhs) =>
  case Import(expr, selectors) =>                                 (eliminated by typecheck)
  case Attributed(attribute, definition) =>                       (eliminated by typecheck)
  case DocDef(comment, definition) =>                             (eliminated by typecheck)
  case Template(parents, body) =>
  case Block(stats, expr) =>
  case CaseDef(pat, guard, body) =>                               (eliminated by transmatch)
  case Sequence(trees) =>                                         (eliminated by transmatch)
  case Alternative(trees) =>                                      (eliminated by transmatch)
  case Bind(name, body) =>                                        (eliminated by transmatch)
  case Function(vparams, body) =>                                 (eliminated by typecheck)
  case Assign(lhs, rhs) =>
  case If(cond, thenp, elsep) =>
  case Match(selector, cases) =>
  case Return(expr) =>
  case Try(block, catches, finalizer) =>
  case Throw(expr) =>
  case New(tpt) =>
  case Typed(expr, tpt) =>                                         (eliminated by erasure)
  case TypeApply(fun, args) =>
  case Apply(fun, args) =>
  case Super(qual, mixin) =>
  case This(qual) =>
  case Select(qualifier, selector) =>
  case Ident(name) =>
  case Literal(value) =>
  case TypeTree() =>
  case SingletonTypeTree(ref) =>                                  (eliminated by typecheck)
  case SelectFromTypeTree(qualifier, selector) =>                 (eliminated by typecheck)
  case CompoundTypeTree(parents, decls) =>                        (eliminated by typecheck)
  case AppliedTypeTree(tpt, args) =>                               (eliminated by typecheck)
*/

  trait TreeCopier {
    def ClassDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], tpt: Tree, impl: Template): ClassDef;
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]): PackageDef;
    def ModuleDef(tree: Tree, mods: int, name: Name, impl: Template): ModuleDef;
    def ValDef(tree: Tree, mods: int, name: Name, tpt: Tree, rhs: Tree): ValDef;
    def DefDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef;
    def AbsTypeDef(tree: Tree, mods: int, name: Name, lo: Tree, hi: Tree): AbsTypeDef;
    def AliasTypeDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], rhs: Tree): AliasTypeDef;
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree): LabelDef;
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]): Import;
    def Attributed(tree: Tree, attribute: Tree, definition: Tree): Attributed;
    def DocDef(tree: Tree, comment: String, definition: Tree): DocDef;
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]): Template;
    def Block(tree: Tree, stats: List[Tree], expr: Tree): Block;
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree): CaseDef;
    def Sequence(tree: Tree, trees: List[Tree]): Sequence;
    def Alternative(tree: Tree, trees: List[Tree]): Alternative;
    def Bind(tree: Tree, name: Name, body: Tree): Bind;
    def Function(tree: Tree, vparams: List[ValDef], body: Tree): Function;
    def Assign(tree: Tree, lhs: Tree, rhs: Tree): Assign;
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree): If;
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]): Match;
    def Return(tree: Tree, expr: Tree): Return;
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree): Try;
    def Throw(tree: Tree, expr: Tree): Throw;
    def New(tree: Tree, tpt: Tree): New;
    def Typed(tree: Tree, expr: Tree, tpt: Tree): Typed;
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]): TypeApply;
    def Apply(tree: Tree, fun: Tree, args: List[Tree]): Apply;
    def Super(tree: Tree, qual: Name, mixin: Name): Super;
    def This(tree: Tree, qual: Name): This;
    def Select(tree: Tree, qualifier: Tree, selector: Name): Select;
    def Ident(tree: Tree, name: Name): Ident;
    def Literal(tree: Tree, value: Any): Literal;
    def TypeTree(tree: Tree): TypeTree;
    def SingletonTypeTree(tree: Tree, ref: Tree): SingletonTypeTree;
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name): SelectFromTypeTree;
    def CompoundTypeTree(tree: Tree, parents: List[Tree], decls: List[Tree]): CompoundTypeTree;
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]): AppliedTypeTree;
  }

  class StrictTreeCopier extends TreeCopier {
    def ClassDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], tpt: Tree, impl: Template) =
      { new ClassDef(mods, name, tparams, tpt, impl).setPos(tree.pos) }
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]) =
      { val t = new PackageDef(name, stats); t.setPos(tree.pos); t }
    def ModuleDef(tree: Tree, mods: int, name: Name, impl: Template) =
      { val t = new ModuleDef(mods, name, impl); t.setPos(tree.pos); t }
    def ValDef(tree: Tree, mods: int, name: Name, tpt: Tree, rhs: Tree) =
      { val t = new ValDef(mods, name, tpt, rhs); t.setPos(tree.pos); t }
    def DefDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) =
      { val t = new DefDef(mods, name, tparams, vparamss, tpt, rhs); t.setPos(tree.pos); t }
    def AbsTypeDef(tree: Tree, mods: int, name: Name, lo: Tree, hi: Tree) =
      { val t = new AbsTypeDef(mods, name, lo, hi); t.setPos(tree.pos); t }
    def AliasTypeDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], rhs: Tree) =
      { val t = new AliasTypeDef(mods, name, tparams, rhs); t.setPos(tree.pos); t }
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) =
      { val t = new LabelDef(name, params, rhs); t.setPos(tree.pos); t }
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]) =
      { val t = new Import(expr, selectors); t.setPos(tree.pos); t }
    def Attributed(tree: Tree, attribute: Tree, definition: Tree) =
      { val t = new Attributed(attribute, definition); t.setPos(tree.pos); t }
    def DocDef(tree: Tree, comment: String, definition: Tree) =
      { val t = new DocDef(comment, definition); t.setPos(tree.pos); t }
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]) =
      { val t = new Template(parents, body); t.setPos(tree.pos); t }
    def Block(tree: Tree, stats: List[Tree], expr: Tree) =
      { val t = new Block(stats, expr); t.setPos(tree.pos); t }
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) =
      { val t = new CaseDef(pat, guard, body); t.setPos(tree.pos); t }
    def Sequence(tree: Tree, trees: List[Tree]) =
      { val t = new Sequence(trees); t.setPos(tree.pos); t }
    def Alternative(tree: Tree, trees: List[Tree]) =
      { val t = new Alternative(trees); t.setPos(tree.pos); t }
    def Bind(tree: Tree, name: Name, body: Tree) =
      { val t = new Bind(name, body); t.setPos(tree.pos); t }
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) =
      { val t = new Function(vparams, body); t.setPos(tree.pos); t }
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) =
      { val t = new Assign(lhs, rhs); t.setPos(tree.pos); t }
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) =
      { val t = new If(cond, thenp, elsep); t.setPos(tree.pos); t }
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =
      { val t = new Match(selector, cases); t.setPos(tree.pos); t }
    def Return(tree: Tree, expr: Tree) =
      { val t = new Return(expr); t.setPos(tree.pos); t }
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) =
      { val t = new Try(block, catches, finalizer); t.setPos(tree.pos); t }
    def Throw(tree: Tree, expr: Tree) =
      { val t = new Throw(expr); t.setPos(tree.pos); t }
    def New(tree: Tree, tpt: Tree) =
      { val t = new New(tpt); t.setPos(tree.pos); t }
    def Typed(tree: Tree, expr: Tree, tpt: Tree) =
      { val t = new Typed(expr, tpt); t.setPos(tree.pos); t }
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
    def TypeTree(tree: Tree) =
      { val t = new TypeTree(); t.setPos(tree.pos); t }
    def SingletonTypeTree(tree: Tree, ref: Tree) =
      { val t = new SingletonTypeTree(ref); t.setPos(tree.pos); t }
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) =
      { val t = new SelectFromTypeTree(qualifier, selector); t.setPos(tree.pos); t }
    def CompoundTypeTree(tree: Tree, parents: List[Tree], decls: List[Tree]) =
      { val t = new CompoundTypeTree(parents, decls); t.setPos(tree.pos); t }
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) =
      { val t = new AppliedTypeTree(tpt, args); t.setPos(tree.pos); t }
  }

  class LazyTreeCopier(copy: TreeCopier) extends TreeCopier {
    def this() = this(new StrictTreeCopier);
    def ClassDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], tpt: Tree, impl: Template) = tree match {
      case t @ ClassDef(mods0, name0, tparams0, tpt0, impl0)
      if (mods0 == mods && (name0 == name) && (tparams0 == tparams) && (tpt0 == tpt) && (impl0 == impl)) => t
      case _ => copy.ClassDef(tree, mods, name, tparams, tpt, impl)
    }
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]) = tree match {
      case t @ PackageDef(name0, stats0)
      if ((name0 == name) && (stats0 == stats)) => t
      case _ => copy.PackageDef(tree, name, stats)
    }
    def ModuleDef(tree: Tree, mods: int, name: Name, impl: Template) = tree match {
      case t @ ModuleDef(mods0, name0, impl0)
      if (mods0 == mods && (name0 == name) && (impl0 == impl)) => t
      case _ => copy.ModuleDef(tree, mods, name, impl)
    }
    def ValDef(tree: Tree, mods: int, name: Name, tpt: Tree, rhs: Tree) = tree match {
      case t @ ValDef(mods0, name0, tpt0, rhs0)
      if (mods0 == mods && (name0 == name) && (tpt0 == tpt) && (rhs0 == rhs)) => t
      case _ => copy.ValDef(tree, mods, name, tpt, rhs)
    }
    def DefDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) = tree match {
      case t @ DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0)
      if (mods0 == mods && (name0 == name) && (tparams0 == tparams) && (vparamss0 == vparamss) && (tpt0 == tpt) && (rhs == rhs0)) => t
      case _ => copy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs)
    }
    def AbsTypeDef(tree: Tree, mods: int, name: Name, lo: Tree, hi: Tree) = tree match {
      case t @ AbsTypeDef(mods0, name0, lo0, hi0)
      if (mods0 == mods && (name0 == name) && (lo0 == lo) && (hi0 == hi)) => t
      case _ => copy.AbsTypeDef(tree, mods, name, lo, hi)
    }
    def AliasTypeDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], rhs: Tree) = tree match {
      case t @ AliasTypeDef(mods0, name0, tparams0, rhs0)
      if (mods0 == mods && (name0 == name) && (tparams0 == tparams) && (rhs0 == rhs)) => t
      case _ => copy.AliasTypeDef(tree, mods, name, tparams, rhs)
    }
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) = tree match {
      case t @ LabelDef(name0, params0, rhs0)
      if ((name0 == name) && (params0 == params) && (rhs0 == rhs)) => t
      case _ => copy.LabelDef(tree, name, params, rhs)
    }
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]) = tree match {
      case t @ Import(expr0, selectors0)
      if ((expr0 == expr) && (selectors0 == selectors)) => t
      case _ => copy.Import(tree, expr, selectors)
    }
    def Attributed(tree: Tree, attribute: Tree, definition: Tree) = tree match {
      case t @ Attributed(attribute0, definition0)
      if ((attribute0 == attribute) && (definition0 == definition)) => t
      case _ => copy.Attributed(tree, attribute, definition)
    }
    def DocDef(tree: Tree, comment: String, definition: Tree) = tree match {
      case t @ DocDef(comment0, definition0)
      if ((comment0 == comment) && (definition0 == definition)) => t
      case _ => copy.DocDef(tree, comment, definition)
    }
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]) = tree match {
      case t @ Template(parents0, body0)
      if ((parents0 == parents) && (body0 == body)) => t
      case _ => copy.Template(tree, parents, body)
    }
    def Block(tree: Tree, stats: List[Tree], expr: Tree) = tree match {
      case t @ Block(stats0, expr0)
      if ((stats0 == stats) && (expr0 == expr)) => t
      case _ => copy.Block(tree, stats, expr)
    }
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) = tree match {
      case t @ CaseDef(pat0, guard0, body0)
      if ((pat0 == pat) && (guard0 == guard) && (body0 == body)) => t
      case _ => copy.CaseDef(tree, pat, guard, body)
    }
    def Sequence(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Sequence(trees0)
      if ((trees0 == trees)) => t
      case _ => copy.Sequence(tree, trees)
    }
    def Alternative(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Alternative(trees0)
      if ((trees0 == trees)) => t
      case _ => copy.Alternative(tree, trees)
    }
    def Bind(tree: Tree, name: Name, body: Tree) = tree match {
      case t @ Bind(name0, body0)
      if ((name0 == name) && (body0 == body)) => t
      case _ => copy.Bind(tree, name, body)
    }
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) = tree match {
      case t @ Function(vparams0, body0)
      if ((vparams0 == vparams) && (body0 == body)) => t
      case _ => copy.Function(tree, vparams, body)
    }
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ Assign(lhs0, rhs0)
      if ((lhs0 == lhs) && (rhs0 == rhs)) => t
      case _ => copy.Assign(tree, lhs, rhs)
    }
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) = tree match {
      case t @ If(cond0, thenp0, elsep0)
      if ((cond0 == cond) && (thenp0 == thenp) && (elsep0 == elsep)) => t
      case _ => copy.If(tree, cond, thenp, elsep)
    }
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =  tree match {
      case t @ Match(selector0, cases0)
      if ((selector0 == selector) && (cases0 == cases)) => t
      case _ => copy.Match(tree, selector, cases)
    }
    def Return(tree: Tree, expr: Tree) = tree match {
      case t @ Return(expr0)
      if ((expr0 == expr)) => t
      case _ => copy.Return(tree, expr)
    }
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) = tree match {
      case t @ Try(block0, catches0, finalizer0)
      if ((block0 == block) && (catches0 == catches) && (finalizer0 == finalizer)) => t
      case _ => copy.Try(tree, block, catches, finalizer)
    }
    def Throw(tree: Tree, expr: Tree) = tree match {
      case t @ Throw(expr0)
      if ((expr0 == expr)) => t
      case _ => copy.Throw(tree, expr)
    }
    def New(tree: Tree, tpt: Tree) = tree match {
      case t @ New(tpt0)
      if ((tpt0 == tpt)) => t
      case _ => copy.New(tree, tpt)
    }
    def Typed(tree: Tree, expr: Tree, tpt: Tree) = tree match {
      case t @ Typed(expr0, tpt0)
      if ((expr0 == expr) && (tpt0 == tpt)) => t
      case _ => copy.Typed(tree, expr, tpt)
    }
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ TypeApply(fun0, args0)
      if ((fun0 == fun) && (args0 == args)) => t
      case _ => copy.TypeApply(tree, fun, args)
    }
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ Apply(fun0, args0)
      if ((fun0 == fun) && (args0 == args)) => t
      case _ => copy.Apply(tree, fun, args)
    }
    def Super(tree: Tree, qual: Name, mixin: Name) = tree match {
      case t @ Super(qual0, mixin0)
      if ((qual0 == qual) && (mixin0 == mixin)) => t
      case _ => copy.Super(tree, qual, mixin)
    }
    def This(tree: Tree, qual: Name) = tree match {
      case t @ This(qual0)
      if ((qual0 == qual)) => t
      case _ => copy.This(tree, qual)
    }
    def Select(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ Select(qualifier0, selector0)
      if ((qualifier0 == qualifier) && (selector0 == selector)) => t
      case _ => copy.Select(tree, qualifier, selector)
    }
    def Ident(tree: Tree, name: Name) = tree match {
      case t @ Ident(name0)
      if ((name0 == name)) => t
      case _ => copy.Ident(tree, name)
    }
    def Literal(tree: Tree, value: Any) = tree match {
      case t @ Literal(value0)
      if (value0 == value) => t
      case _ => copy.Literal(tree, value)
    }
    def TypeTree(tree: Tree) = tree match {
      case t @ TypeTree() => t
      case _ => copy.TypeTree(tree)
    }
    def SingletonTypeTree(tree: Tree, ref: Tree) = tree match {
      case t @ SingletonTypeTree(ref0)
      if ((ref0 == ref)) => t
      case _ => copy.SingletonTypeTree(tree, ref)
    }
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ SelectFromTypeTree(qualifier0, selector0)
      if ((qualifier0 == qualifier) && (selector0 == selector)) => t
      case _ => copy.SelectFromTypeTree(tree, qualifier, selector)
    }
    def CompoundTypeTree(tree: Tree, parents: List[Tree], decls: List[Tree]) = tree match {
      case t @ CompoundTypeTree(parents0, decls0)
      if ((parents0 == parents) && (decls0 == decls)) => t
      case _ => copy.CompoundTypeTree(tree, parents, decls)
    }
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) = tree match {
      case t @ AppliedTypeTree(tpt0, args0)
      if ((tpt0 == tpt) && (args0 == args)) => t
      case _ => copy.AppliedTypeTree(tree, tpt, args)
    }
  }

  class Transformer(val copy: TreeCopier) {
    def this() = this(new LazyTreeCopier);
    def transform(tree: Tree): Tree = tree match {
      case EmptyTree =>
        tree
      case ClassDef(mods, name, tparams, tpt, impl) =>
        copy.ClassDef(tree, mods, name, transformAbsTypeDefs(tparams), transform(tpt), transformTemplate(impl))
      case PackageDef(name, stats) =>
        copy.PackageDef(tree, name, transformTrees(stats))
      case ModuleDef(mods, name, impl) =>
        copy.ModuleDef(tree, mods, name, transformTemplate(impl))
      case ValDef(mods, name, tpt, rhs) =>
        copy.ValDef(tree, mods, name, transform(tpt), transform(rhs))
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        copy.DefDef(tree, mods, name, transformAbsTypeDefs(tparams), transformValDefss(vparamss), transform(tpt), transform(rhs))
      case AbsTypeDef(mods, name, lo, hi) =>
        copy.AbsTypeDef(tree, mods, name, transform(lo), transform(hi))
      case AliasTypeDef(mods, name, tparams, rhs) =>
        copy.AliasTypeDef(tree, mods, name, transformAbsTypeDefs(tparams), transform(rhs))
      case LabelDef(name, params, rhs) =>
        copy.LabelDef(tree, name, transformIdents(params), transform(rhs))
      case Import(expr, selectors) =>
        copy.Import(tree, transform(expr), selectors)
      case Attributed(attribute, definition) =>
        copy.Attributed(tree, transform(attribute), transform(definition))
      case DocDef(comment, definition) =>
        copy.DocDef(tree, comment, transform(definition))
      case Template(parents, body) =>
        copy.Template(tree, transformTrees(parents), transformTrees(body))
      case Block(stats, expr) =>
        copy.Block(tree, transformTrees(stats), transform(expr))
      case CaseDef(pat, guard, body) =>
        copy.CaseDef(tree, transform(pat), transform(guard), transform(body))
      case Sequence(trees) =>
        copy.Sequence(tree, transformTrees(trees))
      case Alternative(trees) =>
        copy.Alternative(tree, transformTrees(trees))
      case Bind(name, body) =>
        copy.Bind(tree, name, transform(body))
      case Function(vparams, body) =>
        copy.Function(tree, transformValDefs(vparams), transform(body))
      case Assign(lhs, rhs) =>
        copy.Assign(tree, transform(lhs), transform(rhs))
      case If(cond, thenp, elsep) =>
        copy.If(tree, transform(cond), transform(thenp), transform(elsep))
      case Match(selector, cases) =>
	copy.Match(tree, transform(selector), transformCaseDefs(cases))
      case Return(expr) =>
	copy.Return(tree, transform(expr))
      case Try(block, catches, finalizer) =>
        copy.Try(tree, transform(block), transformCaseDefs(catches), transform(finalizer))
      case Throw(expr) =>
        copy.Throw(tree, transform(expr))
      case New(tpt) =>
        copy.New(tree, transform(tpt))
      case Typed(expr, tpt) =>
        copy.Typed(tree, transform(expr), transform(tpt))
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
      case TypeTree() =>
        copy.TypeTree(tree)
      case SingletonTypeTree(ref) =>
        copy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        copy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case CompoundTypeTree(parents, decls) =>
        copy.CompoundTypeTree(tree, transformTrees(parents), transformTrees(decls))
      case AppliedTypeTree(tpt, args) =>
        copy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
    }

    def transformTrees(trees: List[Tree]): List[Tree] =
      trees mapConserve (tree => transform(tree));
    def transformTemplate(tree: Template): Template =
      transform(tree: Tree).asInstanceOf[Template];
    def transformAbsTypeDefs(trees: List[AbsTypeDef]): List[AbsTypeDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[AbsTypeDef]);
    def transformValDefs(trees: List[ValDef]): List[ValDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[ValDef]);
    def transformValDefss(treess: List[List[ValDef]]): List[List[ValDef]] =
      treess mapConserve (tree => transformValDefs(tree));
    def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[CaseDef]);
    def transformIdents(trees: List[Ident]): List[Ident] =
      trees mapConserve (tree => transform(tree).asInstanceOf[Ident]);
  }

  class Traverser {
    def traverse(tree: Tree): unit = tree match {
      case ClassDef(mods, name, tparams, tpt, impl) =>
        traverseTrees(tparams); traverse(tpt); traverse(impl)
      case PackageDef(name, stats) =>
        traverseTrees(stats)
      case ModuleDef(mods, name, impl) =>
        traverse(impl)
      case ValDef(mods, name, tpt, rhs) =>
        traverse(tpt); traverse(rhs)
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        traverseTrees(tparams); traverseTreess(vparamss); traverse(tpt); traverse(rhs)
      case AbsTypeDef(mods, name, lo, hi) =>
        traverse(lo); traverse(hi);
      case AliasTypeDef(mods, name, tparams, rhs) =>
        traverseTrees(tparams); traverse(rhs)
      case LabelDef(name, params, rhs) =>
        traverseTrees(params); traverse(rhs)
      case Import(expr, selectors) =>
        traverse(expr)
      case Attributed(attribute, definition) =>
        traverse(attribute); traverse(definition)
      case DocDef(comment, definition) =>
        traverse(definition)
      case Template(parents, body) =>
        traverseTrees(parents); traverseTrees(body)
      case Block(stats, expr) =>
        traverseTrees(stats); traverse(expr)
      case CaseDef(pat, guard, body) =>
        traverse(pat); traverse(guard); traverse(body)
      case Sequence(trees) =>
        traverseTrees(trees)
      case Alternative(trees) =>
        traverseTrees(trees)
      case Bind(name, body) =>
        traverse(body)
      case Function(vparams, body) =>
        traverseTrees(vparams); traverse(body)
      case Assign(lhs, rhs) =>
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
      case Select(qualifier, selector) =>
        traverse(qualifier)
      case SingletonTypeTree(ref) =>
        traverse(ref)
      case SelectFromTypeTree(qualifier, selector) =>
        traverse(qualifier)
      case CompoundTypeTree(parents, decls) =>
        traverseTrees(parents); traverseTrees(decls)
      case AppliedTypeTree(tpt, args) =>
        traverse(tpt); traverseTrees(args)
      case EmptyTree | Super(_, _) | This(_) | Ident(_) | Literal(_) | TypeTree() =>
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

