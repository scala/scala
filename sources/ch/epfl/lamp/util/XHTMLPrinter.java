/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

import java.io.Writer;

/** This class provides methods to print XHTML document.
 */
public class XHTMLPrinter extends HTMLPrinter {

    //########################################################################
    // Public Constructors

    /** Creates a new instance */
    public XHTMLPrinter(Writer writer, String title, String type, String encoding) {
        super(writer, title, type, encoding);
    }

    /** Creates a new instance */
    public XHTMLPrinter(Writer writer, String title, String encoding) {
        super(writer, title, "XHTML 1.0 Transitional", encoding);
    }

    /** Creates a new instance */
    public XHTMLPrinter(Writer writer, String title) {
        super(writer, title, "utf-8");
    }

    //########################################################################
    // Public Methods

    /** Prints text 'text' in bold followed by a new line. */
    public HTMLPrinter printlnBold(String text) {
        printlnTag("span", new Attr[]{ new Attr("style", "font-weight:bold;") }, text);
        return this;
    }

    /** Prints text 'text' in bold. */
    public HTMLPrinter printBold(String text) {
        printTag("span", new Attr[]{ new Attr("style", "font-weight:bold;") }, text);
        return this;
    }

    /** Prints an horizontal line separator
     *  @ param n gives the number of printed blank spaces
     */
    public HTMLPrinter printHLine() {
        printOTag("div", new Attr[] {
            new Attr("style", "border:1px solid #aaaaaa; margin:10px 0px 5px 0px;height:1px;") });
        printlnCTag("div");
        return this;
    }

    /** Prints an horizontal line separator with attributes 'attrs'. */
    public HTMLPrinter printHLine(Attr[] attrs) {
        printHLine();
        return this;
    }

    //########################################################################

    /** Prints XHTML preamble.
     */
    protected void printPreamble() {
	println("<!--");
	println("< ?xml version=\"1.0\" encoding=\"" + getEncoding() + "\"?>");
	println("//-->");
	println("<!DOCTYPE html PUBLIC \"-//W3C//DTD " + getType() +
            "//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">");
	println("<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">").line();
    }

    /** Prints XHTML meta information.
     */
    protected void printMetaInfo(String name) {
	printlnSTag("meta", new Attr[]{
            new Attr("http-equiv", "content-type"),
            new Attr("content", "text/html; charset=" + getEncoding())});
	printlnSTag("meta", new Attr[]{
            new Attr("name", "author"),
            new Attr("content", name)});
    }

    //########################################################################
}
