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

package scala.tools.scalac.ast.printer {


import java.io.OutputStream;
import java.io.PrintWriter;
import java.lang.Math;
import java.util.HashMap;

/**
 * HTML pretty printer for Scala abstract syntax trees.
 *
 * @author Michel Schinz
 * @version 1.0
 */

class HTMLTreePrinter(global0: scalac_Global, out0: PrintWriter)
  extends TextTreePrinter(global0, out0)
{
  protected var outSectionLevel = 1;
  protected var started = false;

  override def begin(): Unit = {
    assert(!started);

    super.begin();
    out.println("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">");
    out.println("<html>");
    out.println("<head>");
    out.println("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">");
    out.println("<link rel=\"stylesheet\" href=\"scala.css\" type=\"text/css\">");
    out.println("<title>Scala tree</title>");
    out.println("</head>");
    out.println("<body>");

    started = true;
  }

  override def end(): Unit = {
    assert(started);

    out.println("</body>");
    out.println("</html>");
    super.end();

    started = false;
  }

  override def beginSection(level: Int, title: String) = {
    outSectionLevel = Math.min(level, 4);
    beginSection1(outSectionLevel, title);
  }

  protected def beginSection1(level: Int, title: String): Unit = {
    if (level == 1)
      out.println("<hr/>");
    val tag: String = "h" + level;
    startTag(tag);
    print(Simple(title));
    endTag(tag);
  }

  protected def startTag(tag: String): Unit = {
    out.print('<'); out.print(tag); out.print('>');
  }

  protected def startTag(tag: String, attr1: String, val1: String): Unit = {
    out.print('<');
    out.print(tag);
    out.print(' ');
    out.print(attr1);
    out.print("=\"");
    out.print(val1);
    out.print("\">");
  }

  protected def endTag(tag: String): Unit = {
    out.print("</"); out.print(tag); out.print(">");
  }

  protected def startSpan(cls: String): Unit = {
    startTag("span", "class", cls);
  }

  protected def endSpan(): Unit = {
    endTag("span");
  }

  override protected def printString(str: String): Unit = {
    for (val i <- Iterator.range(0, str.length())) {
      val c = str.charAt(i);
      val entity: String = c match {
        case '<' => "lt";
        case '>' => "gt";
        case '&' => "amp";
        case _   => null
      }
      if (entity != null) {
        out.print('&');
        out.print(entity);
        out.print(';');
      } else {
        out.print(c);
      }
    }
  }

  protected val symAnchors = new HashMap/*<Symbol,Integer>*/();

  protected def symbolAnchor(sym: Symbol, usage: SymbolUsage): String = {
    var anchorId = symAnchors.get(sym).asInstanceOf[Integer];
    if (anchorId == null) {
      anchorId = new Integer(symAnchors.size());
      symAnchors.put(sym, anchorId);
    }
    if (usage == Definition)
      return anchorId.toString();
    else
      return "#" + anchorId.toString();
  }

  override protected def print(text: Text): Unit = text match {
    case Keyword(name) =>
      startSpan("kw");
      printString(name);
      endSpan();
    case Literal(str) =>
      startSpan("lit");
      printString(str);
      endSpan();
    case Identifier(symbol, name, usage) =>
      val defined = (usage == Definition);
      if (defined) startSpan("idDef");
      if (symbol != null) {
        val attr = if (defined) "name" else "href";
        startTag("a", attr, symbolAnchor(symbol, usage));
        if (usage == Use)
          printString(symbol.simpleName().toString());
        else
          printString(symbol.name.toString());
        endTag("a");
      } else
        printString(name.toString());
      if (defined) endSpan();
    case _ =>
      super.print(text);
  }

  override def print(unit: CompilationUnit) = super.print(unit);
  override def print(str: String) = super.print(str);
  override def print(tree: Tree) = super.print(tree);

  override protected def printUnitHeader(unit: CompilationUnit): Unit = {
    beginSection1(outSectionLevel + 1, unit.source.toString());
    startTag("pre");
  }

  override protected def printUnitFooter(unit: CompilationUnit): Unit = {
    endTag("pre");
  }
}
}
