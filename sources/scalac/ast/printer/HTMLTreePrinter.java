/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scalac.ast.printer;

import scalac.Unit;
import scalac.symtab.Symbol;
import scalac.util.Name;

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

public class HTMLTreePrinter extends TextTreePrinter {
    protected int outSectionLevel = 1;
    protected boolean started = false;

    public HTMLTreePrinter(OutputStream stream) {
        super(stream);
    }

    public void begin() {
        assert !started;

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

    public void end() {
        assert started;

        out.println("</body>");
        out.println("</html>");
        super.end();

        started = false;
    }

    public void beginSection(int level, String title) {
        outSectionLevel = Math.min(level, 4);
        beginSection1(outSectionLevel, title);
    }

    protected void beginSection1(int level, String title) {
        if (level == 1)
            out.println("<hr/>");
        String tag = "h" + level;
        startTag(tag);
        print(Text.Simple(title));
        endTag(tag);
    }

    protected void startTag(String tag) {
        out.print('<'); out.print(tag); out.print('>');
    }

    protected void startTag(String tag, String attr1, String val1) {
        out.print('<');
        out.print(tag);
        out.print(' ');
        out.print(attr1);
        out.print("=\"");
        out.print(val1);
        out.print("\">");
    }

    protected void endTag(String tag) {
        out.print("</"); out.print(tag); out.print(">");
    }

    protected void startSpan(String cls) {
        startTag("span", "class", cls);
    }

    protected void endSpan() {
        endTag("span");
    }

    protected void printString(String str) {
        StringBuffer buf = null;
        int strLen = str.length();

        for (int i = 0; i < strLen; ++i) {
            String entity;
            char c = str.charAt(i);
            switch (c) {
            case '<': entity = "lt"; break;
            case '>': entity = "gt"; break;
            case '&': entity = "amp"; break;
            default:  entity = null; break;
            }
            if (entity != null) {
                out.print('&');
                out.print(entity);
                out.print(';');
            } else
                out.print(c);
        }
    }

    protected static HashMap/*<Symbol,Integer>*/ symAnchors = new HashMap();
    protected String symbolAnchor(Symbol sym, SymbolUsage usage) {
        Integer anchorId = (Integer)symAnchors.get(sym);
        if (anchorId == null) {
            anchorId = new Integer(symAnchors.size());
            symAnchors.put(sym, anchorId);
        }
        if (usage == SymbolUsage.Definition)
            return anchorId.toString();
        else
            return "#" + anchorId.toString();
    }

    protected void print(Text text) {
        switch (text) {
        case Keyword(String name):
            startSpan("kw");
            printString(name);
            endSpan();
            break;
        case Literal(String str):
            startSpan("lit");
            printString(str);
            endSpan();
            break;
        case Identifier(Symbol symbol, Name name, SymbolUsage usage):
            boolean defined = (usage == SymbolUsage.Definition);
            if (defined) startSpan("idDef");
            if (symbol != null) {
                String attr = (defined ? "name" : "href");
                startTag("a", attr, symbolAnchor(symbol, usage));
                if (usage == SymbolUsage.Use)
                    printString(symbol.simpleName().toString());
                else
                    printString(symbol.name.toString());
                endTag("a");
            } else
                printString(name.toString());
            if (defined) endSpan();
            break;
        default:
            super.print(text);
        }
    }

    protected void printUnitHeader(Unit unit) {
        beginSection1(outSectionLevel + 1, unit.source.toString());
        startTag("pre");
    }

    protected void printUnitFooter(Unit unit) {
        endTag("pre");
    }
}
