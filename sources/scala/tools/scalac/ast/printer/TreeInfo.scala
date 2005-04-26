/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$
import scalac.CompilationUnit;
import scalac.symtab._;
import scalac.ast.Tree;

import scalac.{Global => scalac_Global, Phase};
import scalac.CompilationUnit;
import scalac.util.Name;
import scalac.util.TypeNames;

import java.io.OutputStream;
import java.io.PrintWriter;
import java.lang.Math;
import java.util.HashMap;

import javax.swing.tree._;
import javax.swing.event.TreeModelListener;
import javax.swing._;

import java.awt.BorderLayout;

package scala.tools.scalac.ast.printer {


/** Computes different information about a tree node. It
    is used as central place to do all pattern matching against
    Tree.
*/
object TreeInfo {
  val NO_NAME = Name.fromString("");

  /** Return the case class name and the Name, if the node defines one */
  def treeName(t: Tree): Pair[String, Name] = t match {
    case Tree.Attributed(attribute, definition) =>
      Pair("Attributed", NO_NAME);

    case Tree.DocDef(comment, definition) =>
      Pair("DocDef", NO_NAME);

    case Tree.ClassDef(mods, name, tparams, vparams, tpe, impl) =>
      Pair("ClassDef", name);

    case Tree.PackageDef(packaged, impl) =>
      Pair("PackageDef", NO_NAME);

    case Tree.ModuleDef(mods, name, tpe, impl) =>
      Pair("ModuleDef", name);

    case Tree.ValDef(mods, name, tpe, rhs) =>
      Pair("ValDef", name);

    case Tree.PatDef(mods, pat, rhs) =>
      Pair("PatDef", NO_NAME);

    case Tree.DefDef(mods, name, tparams, vparams, tpe, rhs) =>
      Pair("DefDef", name);

    case Tree.AbsTypeDef(mods, name, rhs, lobound) =>
      Pair("AbsTypeDef", name);

    case Tree.AliasTypeDef(mods, name, tparams, rhs) =>
      Pair("AliasTypeDef", name);

    case Tree.Import(expr, selectors) =>
      Pair("Import", Name.fromString(selectors.toString()));

    case Tree.CaseDef(pat, guard, body) =>
      Pair("CaseDef", NO_NAME);

    case Tree.Template(parents, body) =>
      Pair("Template", NO_NAME);

    case Tree.LabelDef(name, params, rhs) =>
      Pair("LabelDef", name);

    case Tree.Block(stats, expr) =>
      Pair("Block", NO_NAME);

    case Tree.Sequence(trees) =>
      Pair("Sequence", NO_NAME);

    case Tree.Alternative(trees) =>
      Pair("Alternative", NO_NAME);

    case Tree.Bind(name, rhs) =>
      Pair("Bind", name);

    case Tree.Visitor(cases) =>
      Pair("Visitor", NO_NAME);

    case Tree.Function(vparams, body) =>
      Pair("Function", NO_NAME);

    case Tree.Assign(lhs, rhs) =>
      Pair("Assign", NO_NAME);

    case Tree.If(cond, thenp, elsep) =>
      Pair("If", NO_NAME);

    case Tree.Switch(test, tags, bodies, otherwise) =>
      Pair("Switch", NO_NAME);

    case Tree.Return(expr) =>
      Pair("Return", NO_NAME);

    case Tree.Throw(expr) =>
      Pair("Throw", NO_NAME);

    case Tree.New(init) =>
      Pair("New", NO_NAME);

    case Tree.Create(qualifier, targs) =>
      Pair("Create", NO_NAME);

    case Tree.Typed(expr, tpe) =>
      Pair("Typed", NO_NAME);

    case Tree.TypeApply(fun, args) =>
      Pair("TypeApply", NO_NAME);

    case Tree.Apply(fun, args) =>
      Pair("Apply", NO_NAME);

    case Tree.Super(qualif, mixin) =>
      Pair("Super", Name.fromString(qualif.toString() + ", mixin: " + mixin.toString()));

    case Tree.This(qualifier) =>
      Pair("This", qualifier);

    case Tree.Select(qualifier, selector) =>
      Pair("Select", selector);

    case Tree.Ident(name) =>
      Pair("Ident", name);

    case Tree.Literal(value) =>
      Pair("Literal", NO_NAME);

    case Tree.TypeTerm() =>
      Pair("TypeTerm", NO_NAME);

    case Tree.SingletonType(ref) =>
      Pair("SingletonType", NO_NAME);

    case Tree.SelectFromType(qualifier, selector) =>
      Pair("SelectFromType", selector);

    case Tree.FunType(argtpes, restpe) =>
      Pair("FunType", NO_NAME);

    case Tree.CompoundType(parents, refinements) =>
      Pair("CompoundType", NO_NAME);

    case Tree.AppliedType(tpe, args) =>
      Pair("AppliedType", NO_NAME);

    case Tree.Try(block, catcher, finalizer) =>
      Pair("Try", NO_NAME);

    case Tree.Empty =>
      Pair("Empty", NO_NAME);
  }

  /** Generate a list that contains all the elements of an Array of Array */
  def flattenArrays[a <: AnyRef](as: Array[Array[a]]): List[a] =  {
    def flattenArrays0[a  <: AnyRef](as: Array[Array[a]], i: Int): List[a] =
      if (as.length - i == 0)
	Nil;
      else if (as.length - i == 1)
	List.fromArray(as(i));
      else
	List.fromArray(as(i)) ::: flattenArrays0(as, i + 1);

    flattenArrays0(as, 0);
  }

  /** Return a list of children for the given tree node */
  def packTreeChildren(t: Tree): List[Tree] = t match {
    case Tree.Attributed(attribute, definition) =>
      List(attribute, definition);

    case Tree.DocDef(comment, definition) =>
      List(definition);

    case Tree.ClassDef(mods, name, tparams, vparams, tpe, impl) => {
      var children: List[Tree] = List();
      children = List.fromArray(tparams) ::: children;
      children = flattenArrays(vparams) ::: children;
      tpe :: impl :: children
    }

    case Tree.PackageDef(packaged, impl) =>
      List(packaged, impl);

    case Tree.ModuleDef(mods, name, tpe, impl) =>
      List(tpe, impl);

    case Tree.ValDef(mods, name, tpe, rhs) =>
      List(tpe, rhs);

    case Tree.PatDef(mods, pat, rhs) =>
      List(pat, rhs);

    case Tree.DefDef(mods, name, tparams, vparams, tpe, rhs) => {
      var children: List[Tree] = List();
      children = List.fromArray(tparams) ::: children;
      children = flattenArrays(vparams) ::: children;
      tpe :: rhs :: children
    }

    case Tree.AbsTypeDef(mods, name, rhs, lobound) =>
      List(rhs, lobound);

    case Tree.AliasTypeDef(mods, name, tparams, rhs) => {
      var children: List[Tree] = List();
      children = List.fromArray(tparams) ::: children;
      rhs :: children
    }

    case Tree.Import(expr, selectors) => {
      var children: List[Tree] = List(expr);
      children
    }

    case Tree.CaseDef(pat, guard, body) =>
      List(pat, guard, body);

    case Tree.Template(parents, body) =>
      List.fromArray(parents) ::: List.fromArray(body);

    case Tree.LabelDef(name, params, rhs) =>
      List.fromArray(params) ::: List(rhs);

    case Tree.Block(stats, expr) =>
      List.fromArray(stats) ::: List(expr);

    case Tree.Sequence(trees) =>
      List.fromArray(trees);

    case Tree.Alternative(trees) =>
      List.fromArray(trees);

    case Tree.Bind(name, rhs) =>
      List(rhs);

    case Tree.Visitor(cases) =>
      List.fromArray(cases);

    case Tree.Function(vparams, body) =>
      List.fromArray(vparams) ::: List(body);

    case Tree.Assign(lhs, rhs) =>
      List(lhs, rhs);

    case Tree.If(cond, thenp, elsep) =>
      List(cond, thenp, elsep);

    case Tree.Switch(test, tags, bodies, otherwise) =>
      test :: List.fromArray(bodies) ::: List(otherwise);

    case Tree.Return(expr) =>
      List(expr);

    case Tree.Throw(expr) =>
      List(expr);

    case Tree.New(init) =>
      List(init);

    case Tree.Create(qualif, targs) =>
      List(qualif) ::: List.fromArray(targs);

    case Tree.Typed(expr, tpe) =>
      List(expr, tpe);

    case Tree.TypeApply(fun, args) =>
      List(fun) ::: List.fromArray(args);

    case Tree.Apply(fun, args) =>
      List(fun) ::: List.fromArray(args);

    case Tree.Super(qualif, mixin) =>
      Nil;

    case Tree.This(qualif) =>
      Nil

    case Tree.Select(qualif, selector) =>
      List(qualif);

    case Tree.Ident(name) =>
      Nil;

    case Tree.Literal(value) =>
      Nil;

    case Tree.TypeTerm() =>
      Nil;

    case Tree.SingletonType(ref) =>
      List(ref);

    case Tree.SelectFromType(qualif, selector) =>
      List(qualif);

    case Tree.FunType(argtpes, restpe) =>
      List.fromArray(argtpes) ::: List(restpe);

    case Tree.CompoundType(parents, refinements) =>
      List.fromArray(parents) ::: List.fromArray(refinements);

    case Tree.AppliedType(tpe, args) =>
      tpe :: List.fromArray(args);

    case Tree.Try(block, catcher, finalizer) =>
      List(block, catcher, finalizer);

    case Tree.Empty =>
      Nil;
  }

  /** Return a textual representation of this t's symbol */
  def symbolText(t: Tree): String = {
    var prefix = "";

    if (t.hasSymbol())
      prefix = "[has] ";
    if (t.definesSymbol())
      prefix = "[defines] ";

    prefix + t.symbol()
  }

  /** Return t's symbol type  */
  def symbolTypeText(t: Tree): String = {
    val s = t.symbol();
    if (s != null)
//      s.`type`().toString();
      TypePrinter.apply(s.getType());
    else
      "";
  }

  /** Return a textual representation of (some of) the symbol's
    * attributes */
  def symbolAttributes(t: Tree): String = {
    val s = t.symbol();
    var att = "";

    if (s != null) {
      if (s.isType())
	att = att + "type ";
      if (s.isFinal())
	att = att + "final ";
      if (s.isSynthetic())
	att = att + "synth ";
      if (s.isExternal())
	att = att + "external ";

      att
    }
    else "";
  }
}

  object TypePrinter {
    def apply(t: Type): String = t match {
      case Type.ThisType(s) => "ThisType(" + s.name + ")\n";
      case Type.SingleType(pre, s) => "SingleType(" +  apply(pre) + ", " + s.name + ")\n";
      case Type.ConstantType(base, value) => "ConstantType(" + apply(base) + ", " + value + ")\n";
      case Type.TypeRef(pre, s, args) => "TypeRef(" + apply(pre) + ", " + s.name + ", " + apply(args) + ")\n)";
      case Type.CompoundType(parts, members) => "CompoundType(" + apply(parts) + ", [members])\n";
      case Type.MethodType(vparams, result) => "MethodType( (" + apply(vparams) + "), " + apply(result) + ")\n";
      case Type.PolyType(tparams, result) => "PolyType( (" + apply(tparams) + "), " + apply(result) + ")\n";
      case Type.OverloadedType(alts, alttypes) => "OverloadedType()";
      case Type.LazyType() => "LazyType()\n";
      case Type.TypeVar(orig, constr) => "TypeVar()";
      case Type.UnboxedType(tag) => "UnboxedType(" + tag + ");\n";
      case Type.UnboxedArrayType(tag) => "UnboxedArrayType(" + tag + ")\n";
      case _ => "<unknown case>[" + t + "]";
    }

    def apply(ts: Array[Type]): String = {
      var s: StringBuffer = new StringBuffer();
      var i: Int = 0;

      while (i < ts.length) {
	s.append(apply(ts(i)));
	if (i != ts.length - 1)
	  s.append(", ");
	i = i + 1;
      }
      s.toString();
    }

    def apply(ts: Array[Symbol]): String = {
      var s: StringBuffer = new StringBuffer();
      var i: Int = 0;

      while (i < ts.length) {
	s.append(apply(ts(i).getType()));
	if (i != ts.length - 1)
	  s.append(", ");
	i = i + 1;
      }
      s.toString();
    }

  }


} // package
