/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scalac.ast.printer;

import scalac.ast.*;
import scalac.symtab.*;
import scalac.util.Debug;
import scalac.Global;
import scalac.Unit;
import scalac.util.Name;

import java.io.*;
import java.util.*;

/**
 * Text pretty printer for Scala abstract syntax trees.
 *
 * @author Michel Schinz, Matthias Zenger
 * @version 1.0
 */
public class TextTreePrinter implements TreePrinter {
    protected PrintWriter out;
    protected final boolean autoFlush;

    protected int indent = 0;
    protected final int INDENT_STEP = 2;
    protected String INDENT_STRING =
        "                                        ";
    protected final int MAX_INDENT = INDENT_STRING.length();

    public TextTreePrinter(OutputStream stream) {
        this(stream, false);
    }

    public TextTreePrinter(OutputStream stream, boolean autoFlush) {
        this.autoFlush = autoFlush;
        this.out = new PrintWriter(stream);
    }

    public TextTreePrinter() {
        this(System.out);
    }

    public void begin() { }

    public void end() {
        flush();
    }

    public void flush() {
	out.flush();
    }

    public TreePrinter print(String str) {
	out.print(str);
        if (autoFlush) flush();
	return this;
    }

    public TreePrinter println() {
	out.println();
        if (autoFlush) flush();
	return this;
    }

    public void beginSection(int level, String title) {
        out.println("[[" + title + "]]");
        flush();
    }

    protected void indent() {
        indent += Math.min(MAX_INDENT, INDENT_STEP);
    }

    protected void undent() {
        indent -= Math.max(0, INDENT_STEP);
    }

    protected void printString(String str) {
        out.print(str);
        if (autoFlush) flush();
    }

    protected void printNewLine() {
        out.println();
	while (indent > INDENT_STRING.length()) {
	    INDENT_STRING = INDENT_STRING + INDENT_STRING;
	}
        if (indent > 0)
            out.write(INDENT_STRING, 0, indent);
        if (autoFlush) flush();
    }

    public static class SymbolUsage {
        public case Definition;
        public case Use;
    }

    public static class Text {
        public case None;
        public case Space;
        public case Newline;
        public case Simple(String str);
        public case Literal(String str);
        public case Keyword(String name);
        public case Identifier(Symbol symbol, String name, SymbolUsage usage);
        public case Sequence(Text[] elements);
    }

    protected void print(Text text) {
        switch (text) {
        case None : break;
        case Space : printString(" "); break;
        case Newline : printNewLine(); break;
        case Simple(String str) : printString(str); break;
        case Literal(String str) : printString(str); break;
        case Keyword(String name) : printString(name); break;
        case Identifier(Symbol sym, String name, _) :
            printString(name);
            if (sym != null && Global.instance.uniqid)
                printString("#" + Global.instance.uniqueID.id(sym));
            break;
        case Sequence(Text[] elements) : print(elements); break;
        }
    }

    protected void print(Text[] texts) {
        for (int i = 0; i < texts.length; ++i)
            print(texts[i]);
    }

    protected static final Text KW_ABSTRACT  = Text.Keyword("abstract");
    protected static final Text KW_CASE      = Text.Keyword("case");
    protected static final Text KW_CLASS     = Text.Keyword("class");
    protected static final Text KW_CONSTR    = Text.Keyword("constr");
    protected static final Text KW_DEF       = Text.Keyword("def");
    protected static final Text KW_DO        = Text.Keyword("do");
    protected static final Text KW_ELSE      = Text.Keyword("else");
    protected static final Text KW_EXTENDS   = Text.Keyword("extends");
    protected static final Text KW_FINAL     = Text.Keyword("final");
    protected static final Text KW_FOR       = Text.Keyword("for");
    protected static final Text KW_IF        = Text.Keyword("if");
    protected static final Text KW_IMPORT    = Text.Keyword("import");
    protected static final Text KW_INTERFACE = Text.Keyword("interface");
    protected static final Text KW_LET       = Text.Keyword("let");
    protected static final Text KW_OBJECT    = Text.Keyword("object");
    protected static final Text KW_NEW       = Text.Keyword("new");
    protected static final Text KW_NULL      = Text.Keyword("null");
    protected static final Text KW_OUTER     = Text.Keyword("outer");
    protected static final Text KW_OVERRIDE  = Text.Keyword("override");
    protected static final Text KW_PACKAGE   = Text.Keyword("package");
    protected static final Text KW_PRIVATE   = Text.Keyword("private");
    protected static final Text KW_PROTECTED = Text.Keyword("protected");
    protected static final Text KW_QUALIFIED = Text.Keyword("qualified");
    protected static final Text KW_STATIC    = Text.Keyword("static");
    protected static final Text KW_SUPER     = Text.Keyword("super");
    protected static final Text KW_THIS      = Text.Keyword("this");
    protected static final Text KW_TYPE      = Text.Keyword("type");
    protected static final Text KW_VAL       = Text.Keyword("val");
    protected static final Text KW_VAR       = Text.Keyword("var");
    protected static final Text KW_WITH      = Text.Keyword("with");
    protected static final Text KW_YIELD     = Text.Keyword("yield");

    protected static final Text TXT_ERROR   = Text.Simple("<error>");
    protected static final Text TXT_UNKNOWN = Text.Simple("<unknown>");
    protected static final Text TXT_NULL    = Text.Simple("<null>");
    protected static final Text TXT_OBJECT_COMMENT
                                            = Text.Simple("/*object*/ ");
    protected static final Text TXT_EMPTY   = Text.Simple("<empty>");

    protected static final Text TXT_QUOTE         = Text.Simple("\"");
    protected static final Text TXT_PLUS          = Text.Simple("+");
    protected static final Text TXT_COLON         = Text.Simple(":");
    protected static final Text TXT_SEMICOLON     = Text.Simple(";");
    protected static final Text TXT_DOT           = Text.Simple(".");
    protected static final Text TXT_COMMA         = Text.Simple(",");
    protected static final Text TXT_EQUAL         = Text.Simple("=");
    protected static final Text TXT_SUBTYPE       = Text.Simple("<:");
    protected static final Text TXT_HASH          = Text.Simple("#");
    protected static final Text TXT_RIGHT_ARROW   = Text.Simple("=>");
    protected static final Text TXT_LEFT_PAREN    = Text.Simple("(");
    protected static final Text TXT_RIGHT_PAREN   = Text.Simple(")");
    protected static final Text TXT_LEFT_BRACE    = Text.Simple("{");
    protected static final Text TXT_RIGHT_BRACE   = Text.Simple("}");
    protected static final Text TXT_LEFT_BRACKET  = Text.Simple("[");
    protected static final Text TXT_RIGHT_BRACKET = Text.Simple("]");

    protected static final Text TXT_WITH_SP =
        Text.Sequence(new Text[]{ Text.Space, KW_WITH, Text.Space });
    protected static final Text TXT_BLOCK_BEGIN =
        Text.Sequence(new Text[]{ TXT_LEFT_BRACE, Text.Newline });
    protected static final Text TXT_BLOCK_END =
        Text.Sequence(new Text[]{ Text.Newline, TXT_RIGHT_BRACE });
    protected static final Text TXT_BLOCK_SEP =
        Text.Sequence(new Text[]{ TXT_SEMICOLON, Text.Newline });
    protected static final Text TXT_COMMA_SP =
        Text.Sequence(new Text[]{ TXT_COMMA, Text.Space });
    protected static final Text TXT_ELSE_NL =
        Text.Sequence(new Text[]{ KW_ELSE, Text.Newline });

    public void print(Unit unit) {
        printUnitHeader(unit);
        if (unit.body != null) {
            for (int i = 0; i < unit.body.length; ++i) {
                print(unit.body[i]);
                print(TXT_BLOCK_SEP);
            }
        } else
            print(TXT_NULL);
        printUnitFooter(unit);

        flush();
    }

    protected void printUnitHeader(Unit unit) {
        print(Text.Simple("// Scala source: " + unit.source + "\n"));
    }

    protected void printUnitFooter(Unit unit) {
        print(Text.Newline);
    }

    public TreePrinter print(Tree tree) {
        switch (tree) {
        case Bad():
            print(TXT_ERROR);
            break;

        case Empty:
            print(TXT_EMPTY);
            break;

        case ClassDef(int mods, // :
                      Name name,
                      Tree.TypeDef[] tparams,
                      Tree.ValDef[][] vparams,
		      Tree tpe,
                      Tree.Template impl):
            printModifiers(mods);
            print((mods & Modifiers.INTERFACE) != 0
                  ? KW_INTERFACE
                  : KW_CLASS);
            print(Text.Space);
            printSymbolDefinition(tree.symbol(), name);
            printParams(tparams);
            printParams(vparams);
            printOpt(TXT_COLON, tpe, false);
            printTemplate(KW_EXTENDS, impl, true);
            break;

        case PackageDef(Tree packaged, Tree.Template impl):
            print(KW_PACKAGE);
            print(Text.Space);
            print(packaged);
            printTemplate(KW_WITH, impl, true);
            break;

        case ModuleDef(int mods, // :
                       Name name,
                       Tree tpe,
                       Tree.Template impl):
            printModifiers(mods);
            print(KW_OBJECT);
            print(Text.Space);
            printSymbolDefinition(tree.symbol(), name);
            printOpt(TXT_COLON, tpe, false);
            printTemplate(KW_EXTENDS, impl, true);
            break;

        case ValDef(int mods, Name name, Tree tpe, Tree rhs):
            printModifiers(mods);
	    if ((mods & Modifiers.MUTABLE) != 0) print(KW_VAR);
	    else {
		if ((mods & Modifiers.MODUL) != 0) print(TXT_OBJECT_COMMENT);
		print(KW_VAL);
	    }
            print(Text.Space);
            printSymbolDefinition(tree.symbol(), name);
            printOpt(TXT_COLON, tpe, false);
	    if ((mods & Modifiers.DEFERRED) == 0) {
		print(Text.Space); print(TXT_EQUAL); print(Text.Space);
		if (rhs == Tree.Empty) print("_");
		else print(rhs);
	    }
            break;

        case PatDef(int mods, Tree pat, Tree rhs):
            printModifiers(mods);
            print(KW_VAL);
            print(Text.Space);
            print(pat);
            printOpt(TXT_EQUAL, rhs, true);
            break;

        case DefDef(int mods,
                    Name name,
                    Tree.TypeDef[] tparams,
                    Tree.ValDef[][] vparams,
                    Tree tpe,
                    Tree rhs):
            printModifiers(mods);
            if (name.isConstrName()) print(KW_CONSTR); else print(KW_DEF);
            print(Text.Space);
            printSymbolDefinition(tree.symbol(), name);
            printParams(tparams);
            printParams(vparams);
            printOpt(TXT_COLON, tpe, false);
            printOpt(TXT_EQUAL, rhs, true);
            break;

        case TypeDef(int mods,
                     Name name,
                     Tree rhs):
            printModifiers(mods);
            print(KW_TYPE);
            print(Text.Space);
            printSymbolDefinition(tree.symbol(), name);
	    if ((mods & (Modifiers.DEFERRED | Modifiers.PARAM)) != 0) printOpt(TXT_SUBTYPE, rhs, true);
            else printOpt(TXT_EQUAL, rhs, true);
            break;

        case Import(Tree expr, Name[] selectors):
            print(KW_IMPORT);
            print(Text.Space);
            print(expr);
	    print(TXT_DOT);
	    print(TXT_LEFT_BRACE);
	    for (int i = 0; i < selectors.length; i = i + 2) {
		if (i > 0) print(TXT_COMMA_SP);
		print(selectors[i].toString());
		if (i + 1 < selectors.length && selectors[i] != selectors[i+1]) {
		    print(TXT_RIGHT_ARROW);
		    print(selectors[i+1].toString());
		}
	    }
	    print(TXT_RIGHT_BRACE);
            break;

        case CaseDef(Tree pat, Tree guard, Tree body):
            print(KW_CASE);
            print(Text.Space);
            print(pat);
            printOpt(KW_IF, guard, true);
            print(Text.Space);
            print(TXT_RIGHT_ARROW);
            print(Text.Space);
            print(body);
            break;

        case LabelDef(Tree[] params, Tree rhs):
            assert tree.symbol() != null;
            printSymbolDefinition(tree.symbol(), null);
            printArray(params, TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);
            print(rhs);
            break;

        case Block(Tree[] stats):
            printArray(stats, TXT_BLOCK_BEGIN, TXT_BLOCK_END, TXT_BLOCK_SEP);
            break;

        case Tuple(Tree[] trees):
            printArray(trees, TXT_LEFT_BRACKET, TXT_RIGHT_BRACKET, TXT_COMMA_SP);
            break;

        case Visitor(Tree.CaseDef[] cases):
            printArray(cases, TXT_BLOCK_BEGIN, TXT_BLOCK_END, Text.Newline);
            break;

        case Function(Tree.ValDef[] vparams, Tree body):
            print(TXT_LEFT_BRACE);
	    printParams(vparams);
	    print(Text.Space);
            print(TXT_RIGHT_ARROW);
            print(Text.Space);
	    print(body);
            print(TXT_RIGHT_BRACE);
            break;

        case Assign(Tree lhs, Tree rhs):
            print(lhs);
            print(Text.Space);
            print(TXT_EQUAL);
            print(Text.Space);
            print(rhs);
            break;

        case If(Tree cond, Tree thenp, Tree elsep):
            print(KW_IF);
            print(Text.Space);
            print(TXT_LEFT_PAREN);
            print(cond);
            print(TXT_RIGHT_PAREN);
            indent(); print(Text.Newline);
            print(thenp);
            undent(); print(Text.Newline);
            indent(); printOpt(TXT_ELSE_NL, elsep, false); undent();
            printType(tree);
            break;

        case New(Tree.Template templ):
            printTemplate(KW_NEW, templ, false);
            printType(tree);
            break;

        case Typed(Tree expr, Tree tpe):
            print(TXT_LEFT_PAREN);
            print(expr);
            print(TXT_RIGHT_PAREN);
            print(Text.Space);
            print(TXT_COLON);
            print(Text.Space);
            print(tpe);
            printType(tree);
            break;

        case TypeApply(Tree fun, Tree[] targs):
            print(fun);
            printArray(targs, TXT_LEFT_BRACKET, TXT_RIGHT_BRACKET, TXT_COMMA_SP);
            printType(tree);
            break;

        case Apply(Tree fun, Tree[] vargs):
            print(fun);
            printArray(vargs, TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);
            printType(tree);
            break;

        case Super(Tree tpe):
            if (tpe != Tree.Empty)
                print(TXT_LEFT_PAREN);

            print(KW_SUPER);

            if (tpe != Tree.Empty) {
                print(Text.Space);
                print(TXT_COLON);
                print(Text.Space);
                print(tpe);
                print(TXT_RIGHT_PAREN);
            }
            printType(tree);
            break;

	case This(Tree qualifier):
	    if (qualifier != Tree.Empty) {
		print(qualifier);
		print(TXT_DOT);
	    }
	    print(KW_THIS);
            printType(tree);
	    break;

        case Select(Tree qualifier, Name name):
            print(qualifier);
            print(TXT_DOT);
            printSymbolUse(tree.symbol(), name);
            printType(tree);
            break;

        case Ident(Name name):
            printSymbolUse(tree.symbol(), name);
            printType(tree);
            break;

        case Literal(Object obj):
            String str;
            if (obj instanceof String)
                str = "\"" + obj + "\"";
            else
                str = String.valueOf(obj);
            print(Text.Literal(str));
            printType(tree);
            break;

	case TypeTerm():
	    print(tree.type.toString());
	    break;

	case SingletonType(Tree ref):
	    print(ref);
	    print(TXT_DOT); print(KW_TYPE);
	    break;

	case SelectFromType(Tree qualifier, Name selector):
	    print(qualifier);
	    print(Text.Space); print(TXT_HASH); print(Text.Space);
	    printSymbolUse(tree.symbol(), selector);
	    break;

        case FunType(Tree[] argtpes, Tree restpe):
            printArray(argtpes, TXT_LEFT_PAREN, TXT_RIGHT_PAREN, TXT_COMMA_SP);
	    print(TXT_RIGHT_ARROW);
            print(restpe);
            break;

        case CompoundType(Tree[] baseTypes, Tree[] refinements):
            printArray(baseTypes, Text.None, Text.None, TXT_WITH_SP);
            printArray(refinements, TXT_BLOCK_BEGIN, TXT_BLOCK_END, Text.Newline);
            break;

        case AppliedType(Tree tpe, Tree[] args):
            print(tpe);
	    indent();
	    print(TXT_LEFT_BRACKET);
	    for (int i = 0; i < args.length; ++i) {
		if (i > 0) print(TXT_COMMA_SP);
		print(args[i]);
	    }
	    undent();
	    print(TXT_RIGHT_BRACKET);
            break;

        case CovariantType(Tree tpe):
	    print(TXT_PLUS);
	    print(tpe);
            break;

        case Template(Tree[] parents, Tree[] body):
            Debug.abort("unexpected case", tree);
            break;

        default:
            print(TXT_UNKNOWN);
            break;
        }
	//print("{" + tree.type + "}");//DEBUG
        if (autoFlush)
            flush();
	return this;
    }

    // Printing helpers

    protected void printArray(Tree[] trees, Text open, Text close, Text sep) {
        indent();
        print(open);
        for (int i = 0; i < trees.length; ++i) {
            if (i > 0) print(sep);
            print(trees[i]);
        }
        undent();
        print(close);
    }

    protected void printOpt(Text prefix, Tree tree, boolean spaceBefore) {
        if (tree != Tree.Empty) {
            if (spaceBefore)
                print(Text.Space);
            print(prefix);
            print(Text.Space);
            print(tree);
        }
    }

    // Printing of symbols

    protected String symbolString(Symbol symbol, Name name) {
        if (symbol != null)
            return symbol.name.toString();
        else
            return name.toString();
    }

    protected void printSymbolDefinition(Symbol symbol, Name name) {
        print(Text.Identifier(symbol,
                              symbolString(symbol, name),
                              SymbolUsage.Definition));
    }

    protected void printSymbolUse(Symbol symbol, Name name) {
        print(Text.Identifier(symbol,
                              symbolString(symbol, name),
                              SymbolUsage.Use));
    }

    // Printing of trees

    protected void printType(Tree tree) {
        if (Global.instance.printtypes) {
	    print(TXT_LEFT_BRACE);
            if (tree.type != null)
                print(Text.Simple(tree.type.toString()));
            else
                print(TXT_NULL);
            print(TXT_RIGHT_BRACE);
	}
    }

    protected void printModifiers(int flags) {
        if ((flags & Modifiers.ABSTRACTCLASS) != 0) {
            print(KW_ABSTRACT);
            print(Text.Space);
        }
        if ((flags & Modifiers.FINAL) != 0) {
            print(KW_FINAL);
            print(Text.Space);
        }
        if ((flags & Modifiers.PRIVATE) != 0) {
            print(KW_PRIVATE);
            print(Text.Space);
        }
        if ((flags & Modifiers.PROTECTED) != 0) {
            print(KW_PROTECTED);
            print(Text.Space);
        }
        if ((flags & Modifiers.QUALIFIED) != 0) {
            print(KW_QUALIFIED);
            print(Text.Space);
        }
        if ((flags & Modifiers.OVERRIDE) != 0) {
            print(KW_OVERRIDE);
            print(Text.Space);
        }
        if ((flags & Modifiers.CASE) != 0) {
            print(KW_CASE);
            print(Text.Space);
        }
        if ((flags & Modifiers.DEF) != 0) {
            print(KW_DEF);
            print(Text.Space);
        }
        if ((flags & Modifiers.STATIC) != 0) {
            print(KW_STATIC);
            print(Text.Space);
        }
    }

    protected void printTemplate(Text prefix,
                                 Tree.Template templ,
                                 boolean spaceBefore) {
        if (! (templ.parents.length == 0
               || (templ.parents.length == 1
                   && templ.parents[0] == Tree.Empty))) {
            if (spaceBefore)
                print(Text.Space);
            print(prefix);
            print(Text.Space);
            printArray(templ.parents, Text.None, Text.None, TXT_WITH_SP);
        }

        if (templ.body.length > 0) {
	    print(Text.Space);
            printArray(templ.body, TXT_BLOCK_BEGIN, TXT_BLOCK_END, TXT_BLOCK_SEP);
	}
    }

    protected void printParams(Tree.TypeDef[] tparams) {
        if (tparams.length > 0) {
	    print(TXT_LEFT_BRACKET);
	    for (int i = 0; i < tparams.length; i++) {
		if (i > 0) print(TXT_COMMA_SP);
		printParam(tparams[i]);
	    }
	    print(TXT_RIGHT_BRACKET);
	}
    }

    protected void printParams(Tree.ValDef[][] vparamss) {
        for (int i = 0; i < vparamss.length; ++i)
            printParams(vparamss[i]);
    }

    protected void printParams(Tree.ValDef[] vparams) {
	print(TXT_LEFT_PAREN);
        for (int i = 0; i < vparams.length; ++i) {
	    if (i > 0) print(TXT_COMMA_SP);
            printParam(vparams[i]);
	}
	print(TXT_RIGHT_PAREN);
    }

    protected void printParam(Tree tree) {
	switch (tree) {
	case TypeDef(int mods, Name name, Tree bound):
            printModifiers(mods);
            printSymbolDefinition(tree.symbol(), name);
            printOpt(TXT_SUBTYPE, bound, true);
            break;

        case ValDef(int mods, Name name, Tree tpe, Tree.Empty):
            printModifiers(mods);
            printSymbolDefinition(tree.symbol(), name);
            printOpt(TXT_COLON, tpe, false);
            break;

	default:
	    Debug.abort("bad parameter: " + tree);
	}
    }
}
