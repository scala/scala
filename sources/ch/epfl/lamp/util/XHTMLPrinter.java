/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

import java.io.Writer;

/**
 * This class provides methods to print XHTML document.
 *
 *  @author     Stephane Micheloud
 *  @version    1.1
 */
public class XHTMLPrinter extends HTMLPrinter {

    //########################################################################
    // Public Constructors

    /**
     * Creates a new instance.
     *
     * @param writer
     * @param title
     * @param representation
     * @param stylesheet
     * @param script
     */
    public XHTMLPrinter(Writer writer, String title, HTMLRepresentation representation,
        String stylesheet, String script)
    {
        super(writer, title, representation, stylesheet, script);
    }

    /**
     * Creates a new instance.
     *
     * @param writer
     * @param title
     * @param representation
     * @param stylesheet
     */
    public XHTMLPrinter(Writer writer, String title, HTMLRepresentation representation,
        String stylesheet)
    {
        this(writer, title, representation, stylesheet, DEFAULT_JAVASCRIPT);
    }

    /**
     * Creates a new instance.
     *
     * @param writer
     * @param title
     * @param repr
     */
    public XHTMLPrinter(Writer writer, String title, HTMLRepresentation representation) {
        this(writer, title, representation, DEFAULT_STYLESHEET);
    }

    /**
     * Creates a new instance with "XHTML 1.0 Transitional" as default document type.
     *
     * @param writer
     * @param title
     * @param docencoding
     */
    public XHTMLPrinter(Writer writer, String title, String docencoding) {
        this(writer, title, new HTMLRepresentation("XHTML 1.0 Transitional", docencoding));
    }

    /**
     * Creates a new instance with "utf-8" as default character encoding.
     *
     * @param writer
     * @param title
     */
    public XHTMLPrinter(Writer writer, String title) {
        this(writer, title, "utf-8");
    }

    //########################################################################
    // Public Methods - Printing simple values followed by a new line

    /**
     * Prints text <code>text</code> in bold followed by a new line.
     *
     * @param text
     * @return the current HTML printer
     */
    public HTMLPrinter printlnBold(String text) {
        return printlnTag("span",
            new XMLAttribute[]{ new XMLAttribute("style", "font-weight:bold;") },
            text);
    }

    /**
     * Prints an horizontal line separator followed by a new line.
     *
     * @return the current HTML printer
     */
    public HTMLPrinter printlnHLine() {
        printOTag("div", new XMLAttribute[] {
            new XMLAttribute("style", "border:1px solid #aaaaaa; " +
                              "margin:10px 0px 5px 0px;height:1px;") });
        return printlnCTag("div");
    }

    //########################################################################
    // Public Methods - Printing simple values

    /**
     * Prints text <code>text</code> in bold.
     *
     * @param text
     * @return the current HTML printer
     */
    public HTMLPrinter printBold(String text) {
        return printTag("span",
            new XMLAttribute[]{ new XMLAttribute("style", "font-weight:bold;") },
            text);
    }

    /**
     * Prints an horizontal line separator
     *
     * @return the current HTML printer
     */
    public HTMLPrinter printHLine() {
        printOTag("div", new XMLAttribute[] {
            new XMLAttribute("style", "border:1px solid #aaaaaa; " +
                              "margin:10px 0px 5px 0px;height:1px;") });
        return printCTag("div");
    }

    /**
     * Prints an horizontal line separator with attributes <code>attrs</code>.
     *
     * @param attrs
     * @return the current HTML printer
     */
    public HTMLPrinter printHLine(XMLAttribute[] attrs) {
        return printHLine();
    }

    /**
     * Prints the &lt;meta/&gt; tag with attributes <code>attrs</code>
     * followed by a new line.
     *
     * @param attrs
     * @return the current HTML printer
     */
    public HTMLPrinter printlnMeta(XMLAttribute[] attrs) {
	return printlnSTag("meta", attrs);
    }

    /**
     * Prints the &lt;link&gt; tag with attributes <code>attrs</code>
     * followed by a new line.
     *
     * @param attrs
     * @return the current HTML printer
     */
    public HTMLPrinter printlnLink(XMLAttribute[] attrs) {
	return printlnSTag("link", attrs);
    }

    //########################################################################

    /**
     * Prints XHTML preamble.
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
