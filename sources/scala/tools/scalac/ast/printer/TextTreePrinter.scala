/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scala.tools.scalac.ast.printer;

import scalac.ast.printer._;
import scalac.ast._;
import scalac.symtab._;
import scalac.util.Debug;
import scalac.Global;
import scalac.Unit;
import scalac.util.Name;
import scalac.util.TypeNames;

import java.io._;

/**
 * Text pretty printer for Scala abstract syntax trees.
 *
 * @author Michel Schinz, Matthias Zenger
 * @version 1.0
 */
class TextTreePrinter(_out: PrintWriter, autoFlush: boolean) with TreePrinter {

  val out = _out;

  def this(stream: OutputStream, autoFlush: boolean) =
    this(new PrintWriter(stream), autoFlush);

  def this(stream: OutputStream) = this(stream, false);

  def this(stream: Writer, autoFlush: boolean) =
    this(new PrintWriter(stream), autoFlush);

  def this(stream: Writer) = this(stream, false);

  def this() = this(System.out);

  protected var indentMargin = 0;
  protected val INDENT_STEP = 2;
  protected var INDENT_STRING =
        "                                        ";
  protected val MAX_INDENT = INDENT_STRING.length();

  def begin() = ();
  def end() = flush();
  def flush() = out.flush();

  def print(str: String) = {
    out.print(str);
    if (autoFlush) flush();
    this
  }

  def println() = {
    out.println();
    if (autoFlush) flush();
    this
  }

  def beginSection(level: int, title: String) = {
    out.println("[[" + title + "]]");
    flush();
  }

  protected def indent() = {
    indentMargin = indentMargin + Math.min(MAX_INDENT, INDENT_STEP);
  }

  protected def undent() = {
    indentMargin = indentMargin - Math.max(0, INDENT_STEP);
  }

  protected def printString(str: String) = {
    out.print(str);
    if (autoFlush) flush();
  }

  protected def printNewLine() = {
    out.println();
    while (indentMargin > INDENT_STRING.length()) {
      INDENT_STRING = INDENT_STRING + INDENT_STRING;
    }
    if (indentMargin > 0)
      out.write(INDENT_STRING, 0, indentMargin);
    if (autoFlush) flush();
  }

  abstract class Text;
  case object None extends Text;
  case object Space extends Text;
  case object Newline extends Text;
  case class Simple(str: String) extends Text;
  case class Literal(str: String) extends Text;
  case class Keyword(name: String) extends Text;
  case class Identifier(symbol: Symbol, name: String, usage: SymbolUsage)
             extends Text;
  case class Sequence(elements: List[Text]) extends Text;

  abstract class SymbolUsage;
  case object Definition extends SymbolUsage;
  case object Use extends SymbolUsage;

  protected def print(text: Text): unit = text match {
    case None => ;
    case Space => printString(" ")
    case Newline => printNewLine()
    case Simple(str) => printString(str)
    case Literal(str) => printString(str)
    case Keyword(name) => printString(name)
    case Identifier(sym, name, _) =>
      printString(name);
      if (sym != null && Global.instance.uniqid)
	printString("#" + Global.instance.uniqueID.id(sym))
    case Sequence(elements) => print(elements)
  }

  protected def print(texts: List[Text]): unit =
    for (val text <- texts) print(text);

  protected final val KW_ABSTRACT  = Keyword("abstract");
  protected final val KW_CASE      = Keyword("case");
  protected final val KW_CLASS     = Keyword("class");
  protected final val KW_DEF       = Keyword("def");
  protected final val KW_DO        = Keyword("do");
  protected final val KW_ELSE      = Keyword("else");
  protected final val KW_EXTENDS   = Keyword("extends");
  protected final val KW_FINAL     = Keyword("final");
  protected final val KW_SEALED    = Keyword("sealed");
  protected final val KW_FOR       = Keyword("for");
  protected final val KW_IF        = Keyword("if");
  protected final val KW_IMPORT    = Keyword("import");
  protected final val KW_INTERFACE = Keyword("interface");
  protected final val KW_OBJECT    = Keyword("object");
  protected final val KW_NEW       = Keyword("new");
  protected final val KW_NULL      = Keyword("null");
  protected final val KW_OUTER     = Keyword("outer");
  protected final val KW_OVERRIDE  = Keyword("override");
  protected final val KW_PACKAGE   = Keyword("package");
  protected final val KW_PRIVATE   = Keyword("private");
  protected final val KW_PROTECTED = Keyword("protected");
  protected final val KW_RETURN    = Keyword("return");
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

  protected final val TXT_QUOTE         = Simple("\"");
  protected final val TXT_PLUS          = Simple("+");
  protected final val TXT_COLON         = Simple(":");
  protected final val TXT_SEMICOLON     = Simple(";");
  protected final val TXT_DOT           = Simple(".");
  protected final val TXT_COMMA         = Simple(",");
  protected final val TXT_EQUAL         = Simple("=");
  protected final val TXT_SUPERTYPE     = Simple(">:");
  protected final val TXT_SUBTYPE       = Simple("<:");
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

  def print(unit: Unit): unit = {
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

  protected def printUnitHeader(unit: Unit): unit =
    print(Simple("// Scala source: " + unit.source + "\n"));

  protected def printUnitFooter(unit: Unit): unit =
    print(Newline);

  def print(tree: Tree): TreePrinter = {
    tree match {
      case Tree$Bad() =>
	print(TXT_ERROR);

      case Tree.Empty =>
	print(TXT_EMPTY);

      case Tree$ClassDef(mods, name, tparams, vparams, tpe, impl) =>
	printModifiers(mods);
	print(if ((mods & Modifiers.INTERFACE) != 0) KW_INTERFACE else KW_CLASS);
	print(Space);
	printSymbolDefinition(tree.symbol(), name);
	printParams(tparams);
	printParams(vparams);
	printOpt(TXT_COLON, tpe, false);
	printTemplate(KW_EXTENDS, impl, true);

      case Tree$PackageDef(packaged, impl) =>
	print(KW_PACKAGE);
	print(Space);
	print(packaged);
	printTemplate(KW_WITH, impl, true);

      case Tree$ModuleDef(mods, name, tpe, impl) =>
	printModifiers(mods);
	print(KW_OBJECT);
	print(Space);
	printSymbolDefinition(tree.symbol(), name);
	printOpt(TXT_COLON, tpe, false);
	printTemplate(KW_EXTENDS, impl, true);

      case Tree$ValDef(mods, name, tpe, rhs) =>
	printModifiers(mods);
	if ((mods & Modifiers.MUTABLE) != 0) {
	  print(KW_VAR);
	} else {
	  if ((mods & Modifiers.MODUL) != 0) print(TXT_OBJECT_COMMENT);
	  print(KW_VAL);
	}
	print(Space);
	printSymbolDefinition(tree.symbol(), name);
	printOpt(TXT_COLON, tpe, false);
	if ((mods & Modifiers.DEFERRED) == 0) {
	  print(Space); print(TXT_EQUAL); print(Space);
	  if (rhs == Tree.Empty) print("_");
	  else print(rhs);
	}

      case Tree$PatDef(mods, pat, rhs) =>
	printModifiers(mods);
	print(KW_VAL);
	print(Space);
	print(pat);
	printOpt(TXT_EQUAL, rhs, true);

      case Tree$DefDef(mods, name, tparams, vparams, tpe, rhs) =>
	printModifiers(mods);
	print(KW_DEF);
	print(Space);
	if (name.isTypeName()) print(KW_THIS);
	else printSymbolDefinition(tree.symbol(), name);
	printParams(tparams);
	printParams(vparams);
	printOpt(TXT_COLON, tpe, false);
	printOpt(TXT_EQUAL, rhs, true);

      case Tree$AbsTypeDef(mods, name, rhs, lobound) =>
	printModifiers(mods);
	print(KW_TYPE);
	print(Space);
	printSymbolDefinition(tree.symbol(), name);
	printBounds(lobound, rhs);

      case Tree$AliasTypeDef(mods, name, tparams, rhs) =>
	printModifiers(mods);
	print(KW_TYPE);
	print(Space);
	printSymbolDefinition(tree.symbol(), name);
	printParams(tparams);
	printOpt(TXT_EQUAL, rhs, true);

      case Tree$Import(expr, selectors) =>
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

      case Tree$CaseDef(pat, guard, body) =>
	print(KW_CASE);
	print(Space);
	print(pat);
	printOpt(KW_IF, guard, true);
	print(Space);
	print(TXT_RIGHT_ARROW);
	print(Space);
	print(body);

      case Tree$LabelDef(name, params, rhs) =>
	printSymbolDefinition(tree.symbol(), name);
	printArray(params.asInstanceOf[Array[Tree]], TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);
	print(rhs);

      case Tree$Block(stats) =>
	printArray(stats, TXT_BLOCK_BEGIN, TXT_BLOCK_END, TXT_BLOCK_SEP);
	printType(tree);

      case Tree$Sequence(trees) =>
	printArray(trees, TXT_LEFT_BRACKET, TXT_RIGHT_BRACKET, TXT_COMMA_SP);

      case Tree$Alternative(trees) =>
	printArray(trees, TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_BAR_SP);

      case Tree$Bind(name, t) =>
	printSymbolDefinition(tree.symbol(), name);
	print(Space);
	print(TXT_AT);
	print(Space);
	print( t );

      case Tree$Visitor(cases) =>
	printArray(cases.asInstanceOf[Array[Tree]], TXT_BLOCK_BEGIN, TXT_BLOCK_END, Newline);

      case Tree$Function(vparams, body) =>
	print(TXT_LEFT_PAREN);
	printParams(vparams);
	print(Space);
	print(TXT_RIGHT_ARROW);
	print(Space);
	print(body);
	print(TXT_RIGHT_PAREN);

      case Tree$Assign(lhs, rhs) =>
	print(lhs);
	print(Space);
	print(TXT_EQUAL);
	print(Space);
	print(rhs);

      case Tree$If(cond, thenp, elsep) =>
	print(KW_IF);
	print(Space);
	print(TXT_LEFT_PAREN);
	print(cond);
	print(TXT_RIGHT_PAREN);
	indent(); print(Newline);
	print(thenp);
	undent(); print(Newline);
	indent(); printOpt(TXT_ELSE_NL, elsep, false); undent();
	printType(tree);

      case Tree$Switch(expr, tags, bodies, defaultBody) =>
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

      case Tree$Return(expr) =>
	print(KW_RETURN);
	print(Space);
	print(expr);

      case Tree$New(templ) =>
	printTemplate(KW_NEW, templ, false);
	printType(tree);

      case Tree$Typed(expr, tpe) =>
	print(TXT_LEFT_PAREN);
	print(expr);
	print(TXT_RIGHT_PAREN);
	print(Space);
	print(TXT_COLON);
	print(Space);
	print(tpe);
	printType(tree);

      case Tree$TypeApply(fun, targs) =>
	print(fun);
	printArray(targs, TXT_LEFT_BRACKET, TXT_RIGHT_BRACKET, TXT_COMMA_SP);
	printType(tree);

      case Tree$Apply(fun, vargs) =>
	if (fun.isInstanceOf[Tree$TypeTerm])
	  print(fun.\"type"/*"*/.resultType().symbol().fullName().toString());
	else
	  print(fun);
	printArray(vargs, TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);
	printType(tree);

      case Tree$Super(qualifier, mixin) =>
	if (qualifier != TypeNames.EMPTY) {
	  printSymbolUse(tree.symbol(), qualifier);
	  print(TXT_DOT);
	}
	print(KW_SUPER);
	if (mixin != TypeNames.EMPTY) {
	  print(TXT_LEFT_PAREN);
	  print(mixin.toString());
	  print(TXT_RIGHT_PAREN);
	}
	printType(tree);

      case Tree$This(name) =>
	if (name != TypeNames.EMPTY) {
	  printSymbolUse(tree.symbol(), name);
	  print(TXT_DOT);
	}
	print(KW_THIS);
	printType(tree);

      case Tree$Select(qualifier, name) =>
	print(qualifier);
	print(TXT_DOT);
	printSymbolUse(tree.symbol(), name);
	printType(tree);

      case Tree$Ident(name) =>
	printSymbolUse(tree.symbol(), name);
	printType(tree);

      case Tree$Literal(obj) =>
	val str = obj match {
	  case s: String    => "\"" + s + "\"";
	  case c: Character => "\'" + c + "\'";
	  case _            => String.valueOf(obj);
	}
	print(Literal(str));
	printType(tree);

      case Tree$TypeTerm() =>
	print(tree.\"type"/*"*/.toString());

      case Tree$SingletonType(ref) =>
	print(ref);
	print(TXT_DOT); print(KW_TYPE);

      case Tree$SelectFromType(qualifier, selector) =>
	print(qualifier);
	print(Space); print(TXT_HASH); print(Space);
	printSymbolUse(tree.symbol(), selector);

      case Tree$FunType(argtpes, restpe) =>
	printArray(argtpes, TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);
	print(TXT_RIGHT_ARROW);
	print(restpe);

      case Tree$CompoundType(baseTypes, refinements) =>
	printArray(baseTypes, None, None, TXT_WITH_SP);
	printArray(refinements, TXT_BLOCK_BEGIN, TXT_BLOCK_END, Newline);

      case Tree$AppliedType(tpe, args) =>
	print(tpe);
	indent();
	print(TXT_LEFT_BRACKET);
	for (val i <- Iterator.range(0, args.length)) {
	  if (i > 0) print(TXT_COMMA_SP);
	  print(args(i));
	}
	undent();
	print(TXT_RIGHT_BRACKET);

      case Tree$Template(parents, body) =>
	Debug.abort("unexpected case: template");

      case _ =>
	print(TXT_UNKNOWN);
    }
    if (autoFlush)
      flush();
    return this;
  }

  // Printing helpers

  protected def printArray(trees: Array[Tree], open: Text, close: Text, sep: Text): unit = {
    indent();
    print(open);
    for (val i <- Iterator.range(0, trees.length)) {
      if (i > 0) print(sep);
      print(trees(i));
    }
    undent();
    print(close);
  }

  protected def printOpt(prefix: Text, tree: Tree, spaceBefore: boolean): unit =
    if (tree != Tree.Empty) {
      if (spaceBefore)
        print(Space);
      print(prefix);
      print(Space);
      print(tree);
    }

  // Printing of symbols

  protected def symbolString(symbol: Symbol, name: Name): String =
    if (symbol != null) symbol.simpleName().toString()
    else name.toString();

  protected def printSymbolDefinition(symbol: Symbol, name: Name): unit =
    print(Identifier(symbol, symbolString(symbol, name), Definition));

  protected def printSymbolUse(symbol: Symbol, name: Name): unit =
    print(Identifier(symbol, symbolString(symbol, name), Use));

  // Printing of trees

  protected def printType(tree: Tree): unit =
    if (Global.instance.printtypes) {
      print(TXT_LEFT_BRACE);
      print(if (tree.\"type"/*"*/ != null) Simple(tree.\"type"/*"*/.toString())
	    else TXT_NULL);
      print(TXT_RIGHT_BRACE);
    }

  protected def printModifiers(flags: int): unit = {
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
    if ((flags & Modifiers.STATIC) != 0) {
      print(KW_STATIC);
      print(Space);
    }
  }

  protected def printTemplate(prefix: Text,
                              templ: Tree$Template,
                              spaceBefore: boolean): unit = {
    if (! (templ.parents.length == 0
           || (templ.parents.length == 1
               && templ.parents(0) == Tree.Empty))) {
      if (spaceBefore)
        print(Space);
      print(prefix);
      print(Space);
      printArray(templ.parents, None, None, TXT_WITH_SP);
    }
    if (templ.body.length > 0) {
      print(Space);
      printArray(templ.body, TXT_BLOCK_BEGIN, TXT_BLOCK_END, TXT_BLOCK_SEP);
    }
  }

  protected def printParams(tparams: Array[Tree$AbsTypeDef]): unit =
    if (tparams.length > 0) {
      print(TXT_LEFT_BRACKET);
      for (val i <- Iterator.range(0, tparams.length)) {
        if (i > 0) print(TXT_COMMA_SP);
        printParam(tparams(i));
      }
      print(TXT_RIGHT_BRACKET);
    }

  protected def printParams(vparamss: Array[Array[Tree$ValDef]]): unit =
    for (val i <- Iterator.range(0, vparamss.length))
      printParams(vparamss(i));

  protected def printParams(vparams: Array[Tree$ValDef]): unit = {
    print(TXT_LEFT_PAREN);
    for (val i <- Iterator.range(0, vparams.length)) {
      if (i > 0) print(TXT_COMMA_SP);
      printParam(vparams(i));
    }
    print(TXT_RIGHT_PAREN);
  }

  protected def printParam(tree: Tree): unit = tree match {
    case Tree$AbsTypeDef(mods, name, bound, lobound) =>
      printModifiers(mods);
      printSymbolDefinition(tree.symbol(), name);
      printBounds(lobound, bound);

    case Tree$ValDef(mods, name, tpe, Tree.Empty) =>
      printModifiers(mods);
      printSymbolDefinition(tree.symbol(), name);
      printOpt(TXT_COLON, tpe, false);

    case _ =>
      Debug.abort("bad parameter: " + tree);
  }

  protected def printBounds(lobound: Tree, hibound: Tree): unit = {
    if (!"scala.All".equals(lobound.toString()))
      printOpt(TXT_SUPERTYPE, lobound, true);
    if (!"scala.Any".equals(hibound.toString()))
      printOpt(TXT_SUBTYPE, hibound, true);
  }
}
