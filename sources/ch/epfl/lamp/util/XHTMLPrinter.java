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
    public XHTMLPrinter(Writer writer, String title, HTMLRepresentation repr) {
        super(writer, title, repr);
    }

    /** Creates a new instance */
    public XHTMLPrinter(Writer writer, String title, String encoding) {
        super(writer, title, encoding);
    }

    /** Creates a new instance */
    public XHTMLPrinter(Writer writer, String title) {
        super(writer, title, "utf-8");
    }

    //########################################################################
    // Public Methods

    /** Prints text 'text' in bold followed by a new line. */
    public HTMLPrinter printlnBold(String text) {
        return printlnTag("span",
            new XMLAttribute[]{ new XMLAttribute("style", "font-weight:bold;") },
            text);
    }

    /** Prints text 'text' in bold. */
    public HTMLPrinter printBold(String text) {
        return printTag("span",
            new XMLAttribute[]{ new XMLAttribute("style", "font-weight:bold;") },
            text);
    }

    /** Prints an horizontal line separator
     *  @ param n gives the number of printed blank spaces
     */
    public HTMLPrinter printHLine() {
        printOTag("div", new XMLAttribute[] {
            new XMLAttribute("style", "border:1px solid #aaaaaa; " +
                              "margin:10px 0px 5px 0px;height:1px;") });
        return printlnCTag("div");
    }

    /** Prints an horizontal line separator with attributes 'attrs'. */
    public HTMLPrinter printHLine(XMLAttribute[] attrs) {
        return printHLine();
    }

    /** Prints the <meta/> tag with attributes 'attrs' followed by a new line. */
    public HTMLPrinter printlnMeta(XMLAttribute[] attrs) {
	return printlnSTag("meta", attrs);
    }

    /** Prints the <link> tag with attributes 'attrs' followed by a new line. */
    public HTMLPrinter printlnLink(XMLAttribute[] attrs) {
	return printlnSTag("link", attrs);
    }

    //########################################################################

    /** Prints XHTML preamble.
     */
    protected void printPreamble() {
	println("<!--");
	println("< ?xml version=\"1.0\" encoding=\"" +
            representation.getEncoding() + "\"?>");
	println("//-->");
	println("<!DOCTYPE html PUBLIC \"-//W3C//DTD " +
            representation.getType() + "//" + representation.getLanguage() +
            "\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">");
	println("<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"" +
            representation.getLanguage() + "\">").line();
    }

    //########################################################################
}
