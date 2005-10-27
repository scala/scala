/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast;

import java.io._;
import symtab.Flags._;

abstract class TreePrinters {

  val global: Global;
  import global._;

  class TreePrinter(out: PrintWriter) {
    protected var indentMargin = 0;
    protected val indentStep = 2;
    protected var indentString = "                                        ";

    def flush = out.flush();

    def indent = indentMargin = indentMargin + indentStep;
    def undent = indentMargin = indentMargin - indentStep;

    def println = {
      out.println();
      while (indentMargin > indentString.length())
        indentString = indentString + indentString;
      if (indentMargin > 0)
        out.write(indentString, 0, indentMargin);
    }

    def printSeq[a](ls: List[a])(printelem: a => unit)(printsep: => unit): unit = ls match {
      case List() =>
      case List(x) => printelem(x)
      case x :: rest => printelem(x); printsep; printSeq(rest)(printelem)(printsep)
    }

    def printColumn(ts: List[Tree], start: String, sep: String, end: String): unit = {
      print(start); indent; println;
      printSeq(ts){print}{print(sep); println}; undent; println; print(end)
    }

    def printRow(ts: List[Tree], start: String, sep: String, end: String): unit = {
      print(start); printSeq(ts){print}{print(sep)}; print(end)
    }

    def printRow(ts: List[Tree], sep: String): unit = printRow(ts, "", sep, "");

    def printTypeParams(ts: List[AbsTypeDef]): unit =
      if (!ts.isEmpty) {
        print("["); printSeq(ts){printParam}{print(", ")}; print("]")
      }

    def printValueParams(ts: List[ValDef]): unit = {
      print("(");
      if (!ts.isEmpty) printModifiers(ts.head.mods & IMPLICIT);
      printSeq(ts){printParam}{print(", ")};
      print(")")
    }

    def printParam(tree: Tree): unit = tree match {
      case ValDef(mods, name, tp, rhs) =>
	print(symName(tree, name)); printOpt(": ", tp);
      case AbsTypeDef(mods, name, lo, hi) =>
        print(symName(tree, name));
        printOpt(" >: ", lo); printOpt(" <: ", hi);
    }

    def printBlock(tree: Tree): unit = tree match {
      case Block(_, _) => print(tree)
      case _ => printColumn(List(tree), "{", ";", "}")
    }

    def symName(tree: Tree, name: Name): String =
      if (tree.symbol != NoSymbol) tree.symbol.nameString else name.toString();

    def printOpt(prefix: String, tree: Tree): unit =
      if (!tree.isEmpty) { print(prefix); print(tree) }

    def printFlags(tree: Tree, flags: long): unit =
      printModifiers(if (tree.symbol == NoSymbol) flags else tree.symbol.flags);

    def printModifiers(flags: long): unit = {
      val mask = if (settings.debug.value) -1 else PrintableFlags;
      val s = flagsToString(flags & mask);
      if (s.length() != 0) print(s + " ")
    }

    def print(str: String): unit = out.print(str);
    def print(name: Name): unit = print(name.toString());

    def printRaw(tree: Tree): unit = {
      tree match {
        case EmptyTree =>
          print("<empty>");

        case ClassDef(mods, name, tparams, tp, impl) =>
          printFlags(tree, mods);
	  print((if ((mods & TRAIT) != 0) "trait " else "class ") + symName(tree, name));
          printTypeParams(tparams);
          printOpt(": ", tp); print(" extends "); print(impl);

        case PackageDef(packaged, stats) =>
          print("package "); print(packaged); printColumn(stats, " {", ";", "}")

        case ModuleDef(mods, name, impl) =>
          printFlags(tree, mods); print("object " + symName(tree, name));
          print(" extends "); print(impl);

        case ValDef(mods, name, tp, rhs) =>
          printFlags(tree, mods);
          print(if ((mods & MUTABLE) != 0) "var " else "val ");
          print(symName(tree, name));
          printOpt(": ", tp);
          if ((mods & DEFERRED) == 0) {
            print(" = ");
            if (rhs.isEmpty) print("_") else print(rhs)
          }

        case DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printFlags(tree, mods);
          print("def " + symName(tree, name));
          printTypeParams(tparams); vparamss foreach printValueParams;
          printOpt(": ", tp); printOpt(" = ", rhs);

        case AbsTypeDef(mods, name, lo, hi) =>
          printFlags(tree, mods); print("type "); printParam(tree);

        case AliasTypeDef(mods, name, tparams, rhs) =>
          printFlags(tree, mods); print("type " + symName(tree, name));
          printTypeParams(tparams); printOpt(" = ", rhs);

        case LabelDef(name, params, rhs) =>
          print(symName(tree, name)); printRow(params, "(", ",", ")"); printBlock(rhs);

        case Import(expr, selectors) =>
          def selectorToString(s: Pair[Name, Name]): String =
            if (s._1 == nme.WILDCARD || s._1 == s._2) s._1.toString()
            else s._1.toString() + "=>" + s._2.toString();
          print("import "); print(expr);
          print(selectors.map(selectorToString).mkString(".{", ", ", "}"))

        case Attributed(attr, definition) =>
          print("["); print(attr); print("]"); println; print(definition);

        case DocDef(comment, definition) =>
          print(comment); println; print(definition);

        case Template(parents, body) =>
          printRow(parents, " with ");
          if (!body.isEmpty) printColumn(body, " {", ";", "}")

        case Block(stats, expr) =>
          printColumn(stats ::: List(expr), "{", ";", "}")

        case Match(selector, cases) =>
          print(selector); printColumn(cases, " match {", "", "}")

        case CaseDef(pat, guard, body) =>
          print("case "); print(pat); printOpt(" if ", guard);
          print(" => "); print(body)

        case Sequence(trees) =>
          printRow(trees, "[", ", ", "]")

        case Alternative(trees) =>
          printRow(trees, "(", "| ", ")")

        case Star(elem) =>
          print("("); print(elem); print(")*");

        case Bind(name, t) =>
          print("("); print(symName(tree, name)); print(" @ "); print(t); print(")");

        case ArrayValue(elemtpt, trees) =>
	  print("Array["); print(elemtpt); printRow(trees, "]{", ", ", "}")

        case Function(vparams, body) =>
          print("("); printValueParams(vparams); print(" => "); print(body); print(")")

        case Assign(lhs, rhs) =>
          print(lhs); print(" = "); print(rhs)

        case If(cond, thenp, elsep) =>
          print("if ("); print(cond); print(")"); indent; println;
          print(thenp); undent;
          if (!elsep.isEmpty) {
            println; print("else"); indent; println; print(elsep); undent
          }

        case Return(expr) =>
          print("return "); print(expr)

        case Try(block, catches, finalizer) =>
          print("try "); printBlock(block);
          if (!catches.isEmpty) printColumn(catches, " catch {", "", "}");
          printOpt(" finally ", finalizer)

        case Throw(expr) =>
          print("throw "); print(expr)

        case New(tpe) =>
          print("new "); print(tpe)

        case Typed(expr, tp) =>
          print("("); print(expr); print(") : "); print(tp);

        case TypeApply(fun, targs) =>
          print(fun); printRow(targs, "[", ", ", "]");

        case Apply(fun, vargs) =>
          print(fun); printRow(vargs, "(", ", ", ")");

        case Super(qual, mixin) =>
          if (qual != nme.EMPTY.toTypeName || tree.symbol != NoSymbol) print(symName(tree, qual) + ".");
          print("super");
          if (mixin != nme.EMPTY.toTypeName)
	    print("[" + mixin + "]")

        case This(qual) =>
          if (qual != nme.EMPTY.toTypeName) print(symName(tree, qual) + ".");
          print("this");

        case Select(qualifier, name) =>
          print(qualifier); print("."); print(symName(tree, name))

        case Ident(name) =>
          print(symName(tree, name))

        case Literal(x) =>
          print(x.tag match {
            case NullTag => "null"
            case StringTag => "\"" + x.stringValue + "\""
            case CharTag => "\'" + x.charValue + "\'"
	    case LongTag => x.longValue.toString() + "L";
            case _ => x.value.toString()
          })

        case TypeTree() =>
	  if (tree.tpe == null) print("<type ?>")
          else print(tree.tpe.toString());

        case SingletonTypeTree(ref) =>
          print(ref); print(".type")

        case SelectFromTypeTree(qualifier, selector) =>
          print(qualifier); print("#"); print(symName(tree, selector))

        case CompoundTypeTree(templ) =>
          print(templ)

        case AppliedTypeTree(tp, args) =>
          print(tp); printRow(args, "[", ", ", "]")
      }
      if (global.settings.printtypes.value && tree.isTerm && !tree.isEmpty) {
        print("{"); print(if (tree.tpe == null) "<null>" else tree.tpe.toString()); print("}")
      }
    }

    def print(tree: Tree): unit =
      printRaw(
        if (tree.isDef && tree.symbol != NoSymbol) {
          tree match {
            case ClassDef(_, _, _, _, impl) => ClassDef(tree.symbol, impl)
            case ModuleDef(_, _, impl)      => ModuleDef(tree.symbol, impl)
            case ValDef(_, _, _, rhs)       => ValDef(tree.symbol, rhs)
            case DefDef(_, _, _, vparamss, _, rhs) => DefDef(tree.symbol, vparamss, rhs)
            case AbsTypeDef(_, _, _, _)     => AbsTypeDef(tree.symbol)
            case AliasTypeDef(_, _, _, rhs) => AliasTypeDef(tree.symbol, rhs)
            case _ => tree
          }
        } else tree);

    def print(unit: CompilationUnit): unit = {
      print("// Scala source: " + unit.source + "\n");
      if (unit.body != null) {
        print(unit.body); println
      } else {
        print("<null>")
      }
      println; flush
    }

    def printAll(): unit = {
      print("[[syntax trees at end of " + phase + "]]");
      for (val unit <- global.currentRun.units) print(unit)
    }
  }

  def create(writer: PrintWriter): TreePrinter = new TreePrinter(writer);
  def create(stream: OutputStream): TreePrinter = create(new PrintWriter(stream));
  def create(): TreePrinter = create(System.out);
}
