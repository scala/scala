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

    def copyAttrs(tree: Tree): this.type = {
      pos = tree.pos;
      tpe = tree.tpe;
      if (hasSymbol) symbol = tree.symbol;
      this
    }
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
  case class CompoundTypeTree(templ: Template)
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
  case CompoundTypeTree(templ: Template) =>                        (eliminated by typecheck)
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
    def CompoundTypeTree(tree: Tree, templ: Template): CompoundTypeTree;
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]): AppliedTypeTree;
  }

  class StrictTreeCopier extends TreeCopier {
    def ClassDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], tpt: Tree, impl: Template) =
      new ClassDef(mods, name, tparams, tpt, impl).copyAttrs(tree);
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]) =
      new PackageDef(name, stats).copyAttrs(tree);
    def ModuleDef(tree: Tree, mods: int, name: Name, impl: Template) =
      new ModuleDef(mods, name, impl).copyAttrs(tree);
    def ValDef(tree: Tree, mods: int, name: Name, tpt: Tree, rhs: Tree) =
      new ValDef(mods, name, tpt, rhs).copyAttrs(tree);
    def DefDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) =
      new DefDef(mods, name, tparams, vparamss, tpt, rhs).copyAttrs(tree);
    def AbsTypeDef(tree: Tree, mods: int, name: Name, lo: Tree, hi: Tree) =
      new AbsTypeDef(mods, name, lo, hi).copyAttrs(tree);
    def AliasTypeDef(tree: Tree, mods: int, name: Name, tparams: List[AbsTypeDef], rhs: Tree) =
      new AliasTypeDef(mods, name, tparams, rhs).copyAttrs(tree);
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) =
      new LabelDef(name, params, rhs).copyAttrs(tree);
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]) =
      new Import(expr, selectors).copyAttrs(tree);
    def Attributed(tree: Tree, attribute: Tree, definition: Tree) =
      new Attributed(attribute, definition).copyAttrs(tree);
    def DocDef(tree: Tree, comment: String, definition: Tree) =
      new DocDef(comment, definition).copyAttrs(tree);
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]) =
      new Template(parents, body).copyAttrs(tree);
    def Block(tree: Tree, stats: List[Tree], expr: Tree) =
      new Block(stats, expr).copyAttrs(tree);
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) =
      new CaseDef(pat, guard, body).copyAttrs(tree);
    def Sequence(tree: Tree, trees: List[Tree]) =
      new Sequence(trees).copyAttrs(tree);
    def Alternative(tree: Tree, trees: List[Tree]) =
      new Alternative(trees).copyAttrs(tree);
    def Bind(tree: Tree, name: Name, body: Tree) =
      new Bind(name, body).copyAttrs(tree);
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) =
      new Function(vparams, body).copyAttrs(tree);
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) =
      new Assign(lhs, rhs).copyAttrs(tree);
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) =
      new If(cond, thenp, elsep).copyAttrs(tree);
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =
      new Match(selector, cases).copyAttrs(tree);
    def Return(tree: Tree, expr: Tree) =
      new Return(expr).copyAttrs(tree);
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) =
      new Try(block, catches, finalizer).copyAttrs(tree);
    def Throw(tree: Tree, expr: Tree) =
      new Throw(expr).copyAttrs(tree);
    def New(tree: Tree, tpt: Tree) =
      new New(tpt).copyAttrs(tree);
    def Typed(tree: Tree, expr: Tree, tpt: Tree) =
      new Typed(expr, tpt).copyAttrs(tree);
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) =
      new TypeApply(fun, args).copyAttrs(tree);
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) =
      new Apply(fun, args).copyAttrs(tree);
    def Super(tree: Tree, qual: Name, mixin: Name) =
      new Super(qual, mixin).copyAttrs(tree);
    def This(tree: Tree, qual: Name) =
      new This(qual).copyAttrs(tree);
    def Select(tree: Tree, qualifier: Tree, selector: Name) =
      new Select(qualifier, selector).copyAttrs(tree);
    def Ident(tree: Tree, name: Name) =
      new Ident(name).copyAttrs(tree);
    def Literal(tree: Tree, value: Any) =
      new Literal(value).copyAttrs(tree);
    def TypeTree(tree: Tree) =
      new TypeTree().copyAttrs(tree);
    def SingletonTypeTree(tree: Tree, ref: Tree) =
      new SingletonTypeTree(ref).copyAttrs(tree);
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) =
      new SelectFromTypeTree(qualifier, selector).copyAttrs(tree);
    def CompoundTypeTree(tree: Tree, templ: Template) =
      new CompoundTypeTree(templ).copyAttrs(tree);
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) =
      new AppliedTypeTree(tpt, args).copyAttrs(tree)
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
    def CompoundTypeTree(tree: Tree, templ: Template) = tree match {
      case t @ CompoundTypeTree(templ0)
      if (templ0 == templ) => t
      case _ => copy.CompoundTypeTree(tree, templ)
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
      case CompoundTypeTree(templ) =>
        copy.CompoundTypeTree(tree, transformTemplate(templ))
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
      case CompoundTypeTree(templ) =>
        traverse(templ)
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

