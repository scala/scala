/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$


import java.io.PrintWriter;
import java.util.ArrayList;

import scalac.CompilationUnit;
import scalac.{Global => scalac_Global, Phase};
import scalac.ast._;
import scalac.ast.printer._;
import scalac.symtab._;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.TypeNames;

package scala.tools.scalac.ast.printer {

/**
 * Text pretty printer for Scala abstract syntax trees.
 *
 * @author Michel Schinz, Matthias Zenger
 * @version 1.0
 */
class TextTreePrinter(global0: scalac_Global, out0: PrintWriter)
  extends TreePrinter
{
  //##########################################################################
  // Public Fields

  val global: scalac_Global = global0;
  val out: PrintWriter = out0;

  //##########################################################################
  // Protected Fields

  protected var indentMargin = 0;
  protected val INDENT_STEP = 2;
  protected var INDENT_STRING = "                                        ";
  protected val MAX_INDENT = INDENT_STRING.length();

  //##########################################################################
  // Public Methods - TreePrinter interface

  def begin(): Unit = ();
  def end(): Unit = flush();
  def flush(): Unit = out.flush();

  def print(units: Array[CompilationUnit]): Unit = printUnits(units);
  def print(tree: Tree): Unit = printTree(tree);

  //##########################################################################

  def beginSection(level: Int, title: String) = {
    out.println("[[" + title + "]]");
    flush();
  }

  protected def indent() = {
    indentMargin = indentMargin + Math.min(MAX_INDENT, INDENT_STEP);
  }

  protected def undent() = {
    indentMargin = indentMargin - Math.max(0, INDENT_STEP);
  }

  protected def print(str: String): Unit = {
    out.print(str);
  }

  protected def printString(str: String) = {
    out.print(str);
  }

  protected def printNewLine() = {
    out.println();
    while (indentMargin > INDENT_STRING.length()) {
      INDENT_STRING = INDENT_STRING + INDENT_STRING;
    }
    if (indentMargin > 0)
      out.write(INDENT_STRING, 0, indentMargin);
  }

  abstract class Text;
  case object None extends Text;
  case object Space extends Text;
  case object Newline extends Text;
  case class Simple(str: String) extends Text;
  case class Literal(str: String) extends Text;
  case class Keyword(name: String) extends Text;
  case class Identifier(symbol: Symbol, name: Name, usage: SymbolUsage)
             extends Text;
  case class Sequence(elements: List[Text]) extends Text;

  abstract class SymbolUsage;
  case object Definition extends SymbolUsage;
  case object Use extends SymbolUsage;

  protected def print(text: Text): Unit = text match {
    case None => ;
    case Space => printString(" ")
    case Newline => printNewLine()
    case Simple(str) => printString(str)
    case Literal(str) => printString(str)
    case Keyword(name) => printString(name)
    case Identifier(sym, name, usage) =>

      if (sym != null) {
        if (usage == Use)
          printString(sym.simpleName().toString());
        else
          printString(sym.name.toString());
        if (global.uniqid)
          printUniqueIdOf(sym)
      } else {
        printString(name.toString());
      }
    case Sequence(elements) => print(elements)
  }

  protected def print(texts: List[Text]): Unit =
    for (val text <- texts) print(text);

  protected final val KW_ABSTRACT  = Keyword("abstract");
  protected final val KW_CASE      = Keyword("case");
  protected final val KW_CLASS     = Keyword("class");
  protected final val KW_DEF       = Keyword("def");
  protected final val KW_DO        = Keyword("do");
  protected final val KW_ELSE      = Keyword("else");
  protected final val KW_EXTENDS   = Keyword("extends");
  protected final val KW_FINAL     = Keyword("final");
  protected final val KW_FOR       = Keyword("for");
  protected final val KW_IF        = Keyword("if");
  protected final val KW_IMPLICIT  = Keyword("implicit");
  protected final val KW_IMPORT    = Keyword("import");
  protected final val KW_INTERFACE = Keyword("interface");
  protected final val KW_MATCH     = Keyword("match");
  protected final val KW_NEW       = Keyword("new");
  protected final val KW_NULL      = Keyword("null");
  protected final val KW_OBJECT    = Keyword("object");
  protected final val KW_OUTER     = Keyword("outer");
  protected final val KW_OVERRIDE  = Keyword("override");
  protected final val KW_PACKAGE   = Keyword("package");
  protected final val KW_PRIVATE   = Keyword("private");
  protected final val KW_PROTECTED = Keyword("protected");
  protected final val KW_RETURN    = Keyword("return");
  protected final val KW_SEALED    = Keyword("sealed");
  protected final val KW_STATIC    = Keyword("static");
  protected final val KW_SUPER     = Keyword("super");
  protected final val KW_THIS      = Keyword("this");
  protected final val KW_TYPE      = Keyword("type");
  protected final val KW_VAL       = Keyword("val");
  protected final val KW_VAR       = Keyword("var");
  protected final val KW_WITH      = Keyword("with");
  protected final val KW_YIELD     = Keyword("yield");

  protected final val TXT_ERROR   = Simple("<error>");
  protected final val TXT_UNKNOWN = Simple("<unknown>");
  protected final val TXT_NULL    = Simple("<null>");
  protected final val TXT_OBJECT_COMMENT = Simple("/*object*/ ");
  protected final val TXT_EMPTY   = Simple("<empty>");
  protected final val TXT_TEMPLATE   = Simple("<template>");

  protected final val TXT_QUOTE         = Simple("\"");
  protected final val TXT_PLUS          = Simple("+");
  protected final val TXT_COLON         = Simple(":");
  protected final val TXT_SEMICOLON     = Simple(";");
  protected final val TXT_DOT           = Simple(".");
  protected final val TXT_COMMA         = Simple(",");
  protected final val TXT_EQUAL         = Simple("=");
  protected final val TXT_SUPERTYPE     = Simple(">:");
  protected final val TXT_SUBTYPE       = Simple("<:");
  protected final val TXT_VIEWBOUND     = Simple("<%");
  protected final val TXT_HASH          = Simple("#");
  protected final val TXT_RIGHT_ARROW   = Simple("=>");
  protected final val TXT_LEFT_PAREN    = Simple("(");
  protected final val TXT_RIGHT_PAREN   = Simple(")");
  protected final val TXT_LEFT_BRACE    = Simple("{");
  protected final val TXT_RIGHT_BRACE   = Simple("}");
  protected final val TXT_LEFT_BRACKET  = Simple("[");
  protected final val TXT_RIGHT_BRACKET = Simple("]");
  protected final val TXT_BAR           = Simple("|");
  protected final val TXT_AT            = Simple("@");

  protected final val TXT_WITH_SP =
    Sequence(List(Space, KW_WITH, Space));
  protected final val TXT_BLOCK_BEGIN =
    Sequence(List(TXT_LEFT_BRACE, Newline));
  protected final val TXT_BLOCK_END =
    Sequence(List(Newline, TXT_RIGHT_BRACE));
  protected final val TXT_BLOCK_SEP =
    Sequence(List(TXT_SEMICOLON, Newline));
  protected final val TXT_COMMA_SP =
    Sequence(List(TXT_COMMA, Space));
  protected final val TXT_ELSE_NL =
    Sequence(List(KW_ELSE, Newline));
  protected final val TXT_BAR_SP =
    Sequence(List(Space, TXT_BAR, Space));

  //##########################################################################
  // Public Methods - Printing Units

  def printUnits(units: Array[CompilationUnit]): Unit = {
    val phase: Phase = global.currentPhase;
    beginSection(1, "syntax trees at " + phase + " (after " + phase.prev + ")");
    for (val i <- Iterator.range(0, units.length))
      printUnit(units(i));
  }

  def printUnit(unit: CompilationUnit): Unit = {
    printUnitHeader(unit);
    if (unit.body != null) {
      for (val i <- Iterator.range(0, unit.body.length)) {
        print(unit.body(i));
        print(TXT_BLOCK_SEP);
      }
    } else {
      print(TXT_NULL);
    }
    printUnitFooter(unit);
    flush();
  }

  def printUnitHeader(unit: CompilationUnit): Unit =
    print(Simple("// Scala source: " + unit.source + "\n"));

  def printUnitFooter(unit: CompilationUnit): Unit =
    print(Newline);

  //##########################################################################
  // Public Methods - Printing trees

  def printTree(tree: Tree): Unit = {
    val symbol = tree.symbol();
    val attributed = symbol != null && (
      !tree.definesSymbol()
      || global.currentPhase.id > global.PHASE.ANALYZER.id()
      || symbol.isInitialized());
    if (attributed) printATree(tree, symbol) else printSTree(tree);
    if (global.printtypes && tree.isTerm()) printTypeOf(tree);
  }

  def printTypeOf(tree: Tree): Unit = {
    val tpe = tree.`type`;
    if (tpe != Type.NoType) {
      print(TXT_LEFT_BRACE);
      if (tpe != null) printType(tpe) else print(TXT_NULL);
      print(TXT_RIGHT_BRACE);
    }
  }

  def printATree(tree: Tree, symbol: Symbol): Unit = tree match {
    case Tree.ClassDef(_, _, _, _, _, impl) =>
      printAModifiers(symbol);
      print(if (symbol.isInterface()) KW_INTERFACE else KW_CLASS);
      print(Space);
      printSymbol(symbol, Definition);
      printAParams(symbol.primaryConstructor().info());
      if (symbol.thisSym() != symbol)
        { print(TXT_COLON); print(Space); printType(symbol.typeOfThis()); }
      printTemplate(symbol, impl);

    case Tree.ModuleDef(_, _, _, impl) =>
      printAModifiers(symbol);
      print(KW_OBJECT);
      print(Space);
      printSymbol(symbol, Definition);
      val clasz = symbol.moduleClass();
      if (clasz.thisSym() != clasz)
        { print(TXT_COLON); print(Space); printType(clasz.typeOfThis()); }
      printTemplate(clasz, impl);

    case Tree.ValDef(_, _, _, rhs) =>
      printAModifiers(symbol);
      if (symbol.isModule()) print(TXT_OBJECT_COMMENT);
      print(if (symbol.isVariable()) KW_VAR else KW_VAL);
      print(Space);
      printSymbol(symbol, Definition);
      print(TXT_COLON);
      print(Space);
      printType(symbol.info());
      if (rhs != Tree.Empty || !symbol.isDeferred()) {
        print(Space); print(TXT_EQUAL); print(Space);
        if (rhs == Tree.Empty) print("_"); else print(rhs);
      }

    case Tree.DefDef(_, _, _, _, _, rhs) =>
      printAModifiers(symbol);
      print(KW_DEF);
      print(Space);
      if (symbol.isConstructor()) print(KW_THIS);
      else printSymbol(symbol, Definition);
      printAParams(symbol.info());
      if (!symbol.isConstructor()) {
        print(TXT_COLON);
        print(Space);
        printType(symbol.info().resultType());
      }
      printOpt(TXT_EQUAL, rhs, true);

    case Tree.AbsTypeDef(_, _, _, _) =>
      printAModifiers(symbol);
      print(KW_TYPE);
      print(Space);
      printSymbol(symbol, Definition);
      printABoundsOf(symbol);

    case Tree.AliasTypeDef(_, _, _, _) =>
      printAModifiers(symbol);
      print(KW_TYPE);
      print(Space);
      printSymbol(symbol, Definition);
      printATypeParams(symbol.typeParams());
      print(Space);
      print(TXT_EQUAL);
      print(Space);
      printType(symbol.info());

    case Tree.Template(_, _) =>
      print(TXT_TEMPLATE);
      printTemplate(symbol.owner(), tree.asInstanceOf[Tree.Template]);

    case Tree.LabelDef(_, params, rhs) =>
      printSymbol(symbol, Definition);
      printArray(params.asInstanceOf[Array[Tree]], TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);
      print(rhs);

    case Tree.Bind(_, rhs) =>
      printSymbol(symbol, Definition);
      print(Space);
      print(TXT_AT);
      print(Space);
      print(rhs);

    case Tree.Return(expr) =>
      print(KW_RETURN);
      printString("/*");
      printSymbol(symbol, Use);
      printString("*/");
      print(Space);
      print(expr);

    case Tree.Create(qualifier, targs) =>
      if (qualifier != Tree.Empty) {
        print(qualifier);
        print(TXT_DOT);
      }
      printSymbol(symbol, Use);
      if (targs.length != 0 || !global.debug)
        printArray(targs, TXT_LEFT_BRACKET, TXT_RIGHT_BRACKET, TXT_COMMA_SP);

    case Tree.Super(_, mixin) =>
      printSymbol(symbol, Use);
      print(TXT_DOT);
      print(KW_SUPER);
      if (mixin != TypeNames.EMPTY) {
        print(TXT_LEFT_PAREN);
        print(mixin.toString());
        print(TXT_RIGHT_PAREN);
      }

    case Tree.This(_) =>
      printSymbol(symbol, Use);
      print(TXT_DOT);
      print(KW_THIS);

    case Tree.Select(qualifier, _) =>
      if (!symbol.owner().isRoot() && !global.debug) {
        print(qualifier);
        print(TXT_DOT);
      }
      printSymbol(symbol, Use);

    case Tree.Ident(_) =>
      printSymbol(symbol, Use);

    case Tree.SelectFromType(qualifier, _) =>
      print(qualifier);
      print(Space); print(TXT_HASH); print(Space);
      printSymbol(symbol, Use);

    case _ =>
      printSTree(tree);
  }

  def printSTree(tree: Tree): Unit = tree match {
    case Tree.Empty =>
      print(TXT_EMPTY);

    case Tree.Attributed(attr, definition) =>
      print(TXT_LEFT_BRACKET);
      print(attr);
      print(TXT_RIGHT_BRACKET);
      printNewLine();
      print(definition);

    case Tree.DocDef(comment, definition) =>
      print(comment);
      printNewLine();
      print(definition);

    case Tree.ClassDef(mods, name, tparams, vparams, tpe, impl) =>
      printSModifiers(mods);
      print(if ((mods & Modifiers.INTERFACE) != 0) KW_INTERFACE else KW_CLASS);
      print(Space);
      printName(tree, name, Definition);
      printSTypeParams(tparams);
      printSValueParams(vparams);
      printOpt(TXT_COLON, tpe, false);
      printTemplate(tree.symbol(), impl);

    case Tree.PackageDef(packaged, impl) =>
      print(KW_PACKAGE);
      print(Space);
      print(packaged);
      printTemplate(null, impl);

    case Tree.ModuleDef(mods, name, tpe, impl) =>
      printSModifiers(mods);
      print(KW_OBJECT);
      print(Space);
      printName(tree, name, Definition);
      printOpt(TXT_COLON, tpe, false);
      printTemplate(null, impl);

    case Tree.ValDef(mods, name, tpe, rhs) =>
      printSModifiers(mods);
      if ((mods & Modifiers.MODUL) != 0) print(TXT_OBJECT_COMMENT);
      print(if ((mods & Modifiers.MUTABLE) != 0) KW_VAR else KW_VAL);
      print(Space);
      printName(tree, name, Definition);
      printOpt(TXT_COLON, tpe, false);
      if ((mods & Modifiers.DEFERRED) == 0) {
        print(Space); print(TXT_EQUAL); print(Space);
        if (rhs == Tree.Empty) print("_");
        else print(rhs);
      }

    case Tree.PatDef(mods, pat, rhs) =>
      printSModifiers(mods);
      print(KW_VAL);
      print(Space);
      print(pat);
      printOpt(TXT_EQUAL, rhs, true);

    case Tree.DefDef(mods, name, tparams, vparams, tpe, rhs) =>
      printSModifiers(mods);
      print(KW_DEF);
      print(Space);
      if (name.isTypeName()) print(KW_THIS);
      else printName(tree, name, Definition);
      printSTypeParams(tparams);
      printSValueParams(vparams);
      printOpt(TXT_COLON, tpe, false);
      printOpt(TXT_EQUAL, rhs, true);

    case Tree.AbsTypeDef(mods, name, rhs, lobound) =>
      printSModifiers(mods);
      print(KW_TYPE);
      print(Space);
      printName(tree, name, Definition);
      printSBounds(lobound, rhs, mods);

    case Tree.AliasTypeDef(mods, name, tparams, rhs) =>
      printSModifiers(mods);
      print(KW_TYPE);
      print(Space);
      printName(tree, name, Definition);
      printSTypeParams(tparams);
      printOpt(TXT_EQUAL, rhs, true);

    case Tree.Import(expr, selectors) =>
      print(KW_IMPORT);
      print(Space);
      print(expr);
      print(TXT_DOT);
      print(TXT_LEFT_BRACE);
      var i = 0;
      while (i < selectors.length) {
        if (i > 0) print(TXT_COMMA_SP);
        print(selectors(i).toString());
        if (i + 1 < selectors.length && selectors(i) != selectors(i+1)) {
          print(TXT_RIGHT_ARROW);
          print(selectors(i+1).toString());
        }
        i = i + 2;
      }
      print(TXT_RIGHT_BRACE);

    case Tree.Template(_, _) =>
      val local = tree.symbol();
      val clasz = if (local != null) local.owner() else null;
      print(TXT_TEMPLATE);
      printTemplate(local, tree.asInstanceOf[Tree.Template]);

    case Tree.CaseDef(pat, guard, body) =>
      print(KW_CASE);
      print(Space);
      print(pat);
      printOpt(KW_IF, guard, true);
      print(Space);
      print(TXT_RIGHT_ARROW);
      print(Space);
      print(body);

    case Tree.LabelDef(name, params, rhs) =>
      printName(tree, name, Definition);
      printArray(params.asInstanceOf[Array[Tree]], TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);
      print(rhs);

    case Tree.Block(stats, value) =>
      printArray(stats, TXT_BLOCK_BEGIN, TXT_SEMICOLON, TXT_BLOCK_SEP);
      indent();
      printNewLine();
      print(value);
      undent();
      print(TXT_BLOCK_END);

    case Tree.Sequence(trees) =>
      printArray(trees, TXT_LEFT_BRACKET, TXT_RIGHT_BRACKET, TXT_COMMA_SP);

    case Tree.Alternative(trees) =>
      printArray(trees, TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_BAR_SP);

    case Tree.Bind(name, t) =>
      printName(tree, name, Definition);
      print(Space);
      print(TXT_AT);
      print(Space);
      print( t );

    case Tree.Visitor(cases) =>
      printArray(cases.asInstanceOf[Array[Tree]], TXT_BLOCK_BEGIN, TXT_BLOCK_END, Newline);

    case Tree.Function(vparams, body) =>
      print(TXT_LEFT_PAREN);
      printSValueParams(vparams);
      print(Space);
      print(TXT_RIGHT_ARROW);
      print(Space);
      print(body);
      print(TXT_RIGHT_PAREN);

    case Tree.Assign(lhs, rhs) =>
      print(lhs);
      print(Space);
      print(TXT_EQUAL);
      print(Space);
      print(rhs);

    case Tree.If(cond, thenp, elsep) =>
      print(KW_IF);
      print(Space);
      print(TXT_LEFT_PAREN);
      print(cond);
      print(TXT_RIGHT_PAREN);
      indent(); print(Newline);
      print(thenp);
      undent(); print(Newline);
      indent(); printOpt(TXT_ELSE_NL, elsep, false); undent();

    case Tree.Switch(expr, tags, bodies, defaultBody) =>
      print("<switch>");
      print(Space);
      print(TXT_LEFT_PAREN);
      print(expr);
      print(TXT_RIGHT_PAREN);
      print(Space);
      indent();
      print(TXT_BLOCK_BEGIN);
      for (val i <- Iterator.range(0, tags.length)) {
        print(KW_CASE);
        print(Space);
        print("" + tags(i));
        print(Space);
        print(TXT_RIGHT_ARROW);
        print(Space);
        print(bodies(i));
        print(Newline);
      }
      print("<default> => ");
      print(defaultBody);
      undent();
      print(TXT_BLOCK_END);

    case Tree.Return(expr) =>
      print(KW_RETURN);
      print(Space);
      print(expr);

    case Tree.New(init) =>
      print(KW_NEW);
      print(Space);
      print(init);

    case Tree.Typed(expr, tpe) =>
      print(TXT_LEFT_PAREN);
      print(expr);
      print(TXT_RIGHT_PAREN);
      print(Space);
      print(TXT_COLON);
      print(Space);
      print(tpe);

    case Tree.TypeApply(fun, targs) =>
      print(fun);
      printArray(targs, TXT_LEFT_BRACKET, TXT_RIGHT_BRACKET, TXT_COMMA_SP);

    case Tree.Apply(fun, vargs) =>
      if (fun.isInstanceOf[Tree.TypeTerm]) {
        val result = fun.`type`.resultType();
        print(Type.appliedType(result, Type.EMPTY_ARRAY).toString());
      }
      else
        print(fun);
      printArray(vargs, TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);

    case Tree.Super(qualifier, mixin) =>
      if (qualifier != TypeNames.EMPTY) {
        printName(tree, qualifier, Use);
        print(TXT_DOT);
      }
      print(KW_SUPER);
      if (mixin != TypeNames.EMPTY) {
        print(TXT_LEFT_PAREN);
        print(mixin.toString());
        print(TXT_RIGHT_PAREN);
      }

    case Tree.This(name) =>
      if (name != TypeNames.EMPTY) {
        printName(tree, name, Use);
        print(TXT_DOT);
      }
      print(KW_THIS);

    case Tree.Select(qualifier, name) =>
      if (global.debug || qualifier.symbol() == null || !qualifier.symbol().isRoot()) {
        print(qualifier);
        print(TXT_DOT);
      }
      printName(tree, name, Use);

    case Tree.Ident(name) =>
      printName(tree, name, Use);

    case Tree.Literal(obj) =>
      print(Literal(obj.toString()));

    case Tree.TypeTerm() =>
      printType(tree.`type`);

    case Tree.SingletonType(ref) =>
      print(ref);
      print(TXT_DOT); print(KW_TYPE);

    case Tree.SelectFromType(qualifier, selector) =>
      print(qualifier);
      print(Space); print(TXT_HASH); print(Space);
      printName(tree, selector, Use);

    case Tree.FunType(argtpes, restpe) =>
      printArray(argtpes, TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);
      print(TXT_RIGHT_ARROW);
      print(restpe);

    case Tree.CompoundType(baseTypes, refinements) =>
      printArray(baseTypes, None, None, TXT_WITH_SP);
      printArray(refinements, TXT_BLOCK_BEGIN, TXT_BLOCK_END, Newline);

    case Tree.AppliedType(tpe, args) =>
      print(tpe);
      indent();
      print(TXT_LEFT_BRACKET);
      for (val i <- Iterator.range(0, args.length)) {
        if (i > 0) print(TXT_COMMA_SP);
        print(args(i));
      }
      undent();
      print(TXT_RIGHT_BRACKET);

    case _ =>
      print(TXT_UNKNOWN);
  }

  //##########################################################################
  // Public Methods - Printing symbols and names

  /** Print symbol. */
  def printSymbol(symbol: Symbol, usage: SymbolUsage): Unit =
    print(Identifier(symbol, symbol.name, usage));

  /** Print unique identifier of symbol */
  def printUniqueIdOf(symbol: Symbol): Unit = {
    print(TXT_HASH);
    printString(String.valueOf(symbol.id));
  }

  /** Print name. */
  def printName(name: Name, usage: SymbolUsage): Unit =
    print(Identifier(null, name, usage));

  /** Print name. */
  def printName(tree: Tree, name: Name, usage: SymbolUsage): Unit =
    if (tree.symbol() != null) printSymbol(tree.symbol(), usage)
    else printName(name, usage);

  //##########################################################################
  // Public Methods - Printing modifiers

  /** Print syntactic modifiers. */
  def printSModifiers(flags: Int): Unit = {
    if ((flags & Modifiers.ABSTRACT) != 0) {
      print(KW_ABSTRACT);
      print(Space);
    }
    if ((flags & Modifiers.FINAL) != 0) {
      print(KW_FINAL);
      print(Space);
    }
    if ((flags & Modifiers.SEALED) != 0) {
      print(KW_SEALED);
      print(Space);
    }
    if ((flags & Modifiers.PRIVATE) != 0) {
      print(KW_PRIVATE);
      print(Space);
    }
    if ((flags & Modifiers.PROTECTED) != 0) {
      print(KW_PROTECTED);
      print(Space);
    }
    if ((flags & Modifiers.OVERRIDE) != 0) {
      print(KW_OVERRIDE);
      print(Space);
    }
    if ((flags & Modifiers.CASE) != 0) {
      print(KW_CASE);
      print(Space);
    }
    if ((flags & Modifiers.DEF) != 0) {
      print(KW_DEF);
      print(Space);
    }
  }

  /** Print attributed modifiers. */
  def printAModifiers(symbol: Symbol): Unit = {
    if (symbol.isAbstractClass()) {
      print(KW_ABSTRACT);
      print(Space);
    }
    if (symbol.isFinal()) {
      print(KW_FINAL);
      print(Space);
    }
    if (symbol.isSealed()) {
      print(KW_SEALED);
      print(Space);
    }
    if (symbol.isPrivate()) {
      print(KW_PRIVATE);
      print(Space);
    }
    if (symbol.isProtected()) {
      print(KW_PROTECTED);
      print(Space);
    }
    if (symbol.isOverride()) {
      print(KW_OVERRIDE);
      print(Space);
    }
    if (symbol.isCaseClass()) {
      print(KW_CASE);
      print(Space);
    }
    if (symbol.isDefParameter()) {
      print(KW_DEF);
      print(Space);
    }
  }

  //##########################################################################
  // Public Methods - Printing templates

  /** Print template. */
  def printTemplate(clasz: Symbol, template: Tree.Template): Unit = {
    val local = template.symbol();
    val parents = template.parents;
    val body = template.body;
    val types = new ArrayList();
    // collect type members
    if (clasz != null) {
      if (global.currentPhase.id > global.PHASE.ADDACCESSORS.id()) {
        val i = clasz.members().iterator();
        while (i.hasNext()) {
          val member = i.next();
          if (member.isTypeAlias() || member.isAbstractType())
            types.add(member);
        }
      }
    }
    // print parents
    if (parents.length > 0) {
      print(Space);
      print(KW_EXTENDS);
      print(Space);
      printArray(parents, None, None, TXT_WITH_SP);
    }
    // print local
    if (global.uniqid && local != null) {
      print(Space);
      printString("/*");
      printString("local");
      printUniqueIdOf(local);
      printString("*/");
    }
    // print body
    if (body.length > 0 || types.size() > 0) {
      print(Space);
      indent();
      print(TXT_BLOCK_BEGIN);
      if (types.size() > 0) {
        val printer =
          new SymbolTablePrinter(INDENT_STRING.substring(0, indentMargin));
        printer.indent();
        var i: Int = 0;
        while (i < types.size()) {
          if (i > 0) printer.line();
          val type0 = types.get(i).asInstanceOf[Symbol];
          printer.printSignature(type0).print(';');
          i = i + 1;
        }
        printer.undent();
        print(printer.toString().substring(indentMargin));
        print(Newline);
      }
      var i: Int = 0;
      while (i < body.length) {
        if (i > 0) print(TXT_BLOCK_SEP);
        print(body(i));
        i = i + 1;
      }
      undent();
      print(TXT_BLOCK_END);
    }
  }

  //##########################################################################
  // Public Methods - Printing parameters

  /** Print attributed parameter lists. */
  def printAParams(tpe: Type): Unit = tpe match {
    case Type.PolyType(params, result) =>
      printATypeParams(params);
      printAParams(result);
    case Type.MethodType(params, result) =>
      printAValueParams(params);
      printAParams(result);
    case _ =>
  }

  /** Print attributed type parameter list. */
  def printATypeParams(tparams: Array[Symbol]): Unit =
    if (tparams.length > 0 && !global.debug) {
      print(TXT_LEFT_BRACKET);
      for (val i <- Iterator.range(0, tparams.length)) {
        if (i > 0) print(TXT_COMMA_SP);
        printATypeParam(tparams(i));
      }
      print(TXT_RIGHT_BRACKET);
    }

  /** Print attributed type parameter. */
  def printATypeParam(tparam: Symbol): Unit = {
    printAModifiers(tparam);
    printSymbol(tparam, Definition);
    printABoundsOf(tparam);
  }

  /** Print attributed value parameter list. */
  def printAValueParams(vparams: Array[Symbol]): Unit = {
    print(TXT_LEFT_PAREN);
    for (val i <- Iterator.range(0, vparams.length)) {
      if (i > 0) print(TXT_COMMA_SP);
      printAValueParam(vparams(i));
    }
    print(TXT_RIGHT_PAREN);
  }

  /** Print attributed value parameter. */
  def printAValueParam(vparam: Symbol): Unit = {
    printAModifiers(vparam);
    if (vparam.hasParamAccessorFlag()) {
      print(KW_VAL); print(Space);
    }
    printSymbol(vparam, Definition);
    print(TXT_COLON);
    print(Space);
    printType(vparam.info());
  }

  /** Print syntactic type parameter list. */
  def printSTypeParams(tparams: Array[Tree.AbsTypeDef]): Unit =
    if (tparams.length > 0) {
      print(TXT_LEFT_BRACKET);
      for (val i <- Iterator.range(0, tparams.length)) {
        if (i > 0) print(TXT_COMMA_SP);
        printSTypeParam(tparams(i));
      }
      print(TXT_RIGHT_BRACKET);
    }

  /** Print syntactic type parameter. */
  def printSTypeParam(tparam: Tree.AbsTypeDef): Unit = {
    printSModifiers(tparam.mods);
    printName(tparam, tparam.name, Definition); // !!!
    printSBounds(tparam.lobound, tparam.rhs, tparam.mods);
  }

  /** Print syntactic value parameter lists. */
  def printSValueParams(vparamss: Array[Array[Tree.ValDef]]): Unit =
    for (val i <- Iterator.range(0, vparamss.length))
      printSValueParams(vparamss(i));

  /** Print syntactic value parameter list. */
  def printSValueParams(vparams: Array[Tree.ValDef]): Unit = {
    print(TXT_LEFT_PAREN);
    for (val i <- Iterator.range(0, vparams.length)) {
      if (i > 0) print(TXT_COMMA_SP);
      printSValueParam(vparams(i));
    }
    print(TXT_RIGHT_PAREN);
  }

  /** Print syntactic value parameter. */
  def printSValueParam(vparam: Tree.ValDef): Unit = {
    printSModifiers(vparam.mods);
    if ((vparam.mods & Modifiers.PARAMACCESSOR) != 0) {
      print(KW_VAL); print(Space);
    }
    printName(vparam, vparam.name, Definition); // !!!
    printOpt(TXT_COLON, vparam.tpe, false);
  }

  //##########################################################################
  // Public Methods - Printing types and bounds

  /** Print attributed type. */
  def printType(tp: Type): Unit = {
    print(tp.toString());
  }

  /** Print attributed bound. */
  def printABound(kind: Text, bound: Type, default: Symbol): Unit = {
    if (bound.symbol() != default && !global.debug) {
      print(Space);
      print(kind);
      print(Space);
      printType(bound);
    }
  }

  /** Print attributed lower bound. */
  def printALoBound(lobound: Type): Unit =
    printABound(TXT_SUPERTYPE, lobound, global.definitions.ALL_CLASS);

  /** Print attributed view bound. */
  def printAVuBound(vubound: Type): Unit =
    printABound(TXT_VIEWBOUND, vubound, global.definitions.ANY_CLASS);

  /** Print attributed higher bound. */
  def printAHiBound(hibound: Type): Unit =
    printABound(TXT_SUBTYPE, hibound, global.definitions.ANY_CLASS);

  /** Print attributed bounds of symbol. */
  def printABoundsOf(symbol: Symbol): Unit = {
    printALoBound(symbol.loBound());
    if (symbol.isViewBounded()) printAVuBound(symbol.vuBound());
    printAHiBound(symbol.info());
  }

  /** Print syntactic bounds. */
  def printSBounds(lobound: Tree, hibound: Tree, mods: Int): Unit = {
    if (lobound.getType() != null)
      printALoBound(lobound.getType());
    else if (!"scala.All".equals(lobound.toString()) && !global.debug)
      printOpt(TXT_SUPERTYPE, lobound, true);
    val isViewBounded = (mods & Modifiers.VIEWBOUND) != 0;
    if (hibound.getType() != null)
      if (isViewBounded)
        printAVuBound(hibound.getType());
      else
        printAHiBound(hibound.getType());
    else if (!"scala.Any".equals(hibound.toString()) && !global.debug)
      printOpt(if (isViewBounded) TXT_VIEWBOUND else TXT_SUBTYPE,hibound,true);
  }

  //##########################################################################
  // Public Methods - Helper methods

  def printArray(trees: Array[Tree], open: Text, close: Text, sep: Text):Unit={
    indent();
    print(open);
    for (val i <- Iterator.range(0, trees.length)) {
      if (i > 0) print(sep);
      print(trees(i));
    }
    undent();
    print(close);
  }

  def printOpt(prefix: Text, tree: Tree, spaceBefore: boolean): Unit =
    if (tree != Tree.Empty) {
      if (spaceBefore)
        print(Space);
      print(prefix);
      print(Space);
      print(tree);
    }

  //##########################################################################
}
}
