/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

import java.io.Writer;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * This class provides methods to print HTML document.
 *
 *  @author     Stephane Micheloud
 *  @version    1.1
 */
public class HTMLPrinter {

    public static final String DEFAULT_STYLESHEET = "style.css";

    //########################################################################
    // Private Fields

    /** The underlying code printer */
    private final CodePrinter printer;

    /** The document title */
    private final String title;

    //########################################################################
    // Protected Fields

    /** The document representation */
    protected final HTMLRepresentation representation;

    //########################################################################
    // Public Constructors

    /** Creates a new instance */
    public HTMLPrinter(Writer writer, String title, HTMLRepresentation representation) {
        this.printer = new CodePrinter(writer, "  ");
        this.title = title;
        this.representation = representation;
    }

    /** Creates a new instance */
    public HTMLPrinter(Writer writer, String title, String encoding) {
        this.printer = new CodePrinter(writer, "  ");
        this.title = title;
        this.representation = new HTMLRepresentation("HTML 4.01 Transitional", encoding);
    }

    /** Creates a new instance */
    public HTMLPrinter(Writer writer, String title) {
        this(writer, title,  "iso-8859-1");
    }

    //########################################################################
    // Public Methods - Getting & Setting

    /** Returns the underlying code printer. */
    public CodePrinter getCodePrinter() {
        return printer;
    }

    /** Returns the underlying title. */
    public String getTitle() {
        return title;
    }

    //########################################################################
    // Public Methods - Formatting

    /** Increases the indentation level by one. */
    public HTMLPrinter indent() {
        printer.indent();
        return this;
    }

    /** Decreases the indentation level by one. */
    public HTMLPrinter undent() {
        printer.undent();
        return this;
    }

    /** Inserts a new line. */
    public HTMLPrinter line() {
        printer.line();
        return this;
    }

    /** Inserts a white space. */
    public HTMLPrinter space() {
        printer.space();
        return this;
    }

    //########################################################################
    // Public Methods - Printing simple values followed by a new line

    /** Prints a new line. */
    public HTMLPrinter println() {
        printer.println();
        return this;
    }

    /** Prints the boolean value followed by a new line. */
    public HTMLPrinter println(boolean value) {
        printer.println(value);
        return this;
    }

    /** Prints the byte value followed by a new line. */
    public HTMLPrinter println(byte value) {
        printer.println(value);
        return this;
    }

    /** Prints the short value followed by a new line. */
    public HTMLPrinter println(short value) {
        printer.println(value);
        return this;
    }

    /** Prints the char value followed by a new line. */
    public HTMLPrinter println(char value) {
        printer.println(value);
        return this;
    }

    /** Prints the int value followed by a new line. */
    public HTMLPrinter println(int value) {
        printer.println(value);
        return this;
    }

    /** Prints the long value followed by a new line. */
    public HTMLPrinter println(long value) {
        printer.println(value);
        return this;
    }

    /** Prints the float value followed by a new line. */
    public HTMLPrinter println(float value) {
        printer.println(value);
        return this;
    }

    /** Prints the double value followed by a new line. */
    public HTMLPrinter println(double value) {
        printer.println(value);
        return this;
    }

    /** Prints the string followed by a new line. */
    public HTMLPrinter println(String value) {
        printer.println(value);
        return this;
    }

    /** Prints the opening HTML tag followed by a new line. */
    public HTMLPrinter printlnOTag(String label) {
        printer.print("<");
        printer.print(label);
        printer.println(">");
        return this;
    }

    /** Prints the HTML tag with attributes followed by a new line. */
    public HTMLPrinter printlnOTag(String label, XMLAttribute[] attrs) {
        printer.print("<");
        printer.print(label);
        printer.print(" ");
        printer.print(StringOf.object.array(attrs, "", " ", ""));
        printer.println(">");
        return this;
    }

    /** Prints the closing HTML tag followed by a new line. */
    public HTMLPrinter printlnCTag(String label) {
        printer.print("</");
        printer.print(label);
        printer.println(">");
        return this;
    }

    /** Prints HTML tag with label and contents followed by a new line. */
    public HTMLPrinter printlnTag(String label, String text) {
        printOTag(label);
        print(text);
        return printlnCTag(label);
    }

    /** Prints the HTML tag 'label' with attributes 'attrs' and contents 'text'
     *  followed by a new line.
     */
    public HTMLPrinter printlnTag(String label, XMLAttribute[] attrs, String text) {
        printOTag(label, attrs);
        print(text);
        return printlnCTag(label);
    }

    /** Prints the short HTML tag followed by a new line.
     *  @param label
     */
    public HTMLPrinter printlnSTag(String label) {
        printer.print("<");
        printer.print(label);
        printer.println("/>");
        return this;
    }

    /** Prints the short HTML tag with attributes 'attrs' followed by a new line. */
    public HTMLPrinter printlnSTag(String label, XMLAttribute[] attrs) {
        printer.print("<");
        printer.print(label);
        printer.print(" ");
        printer.print(StringOf.object.array(attrs, "", " ", ""));
        printer.println("/>");
        return this;
    }

    /** Prints <A HREF=link> text </a> tag followed by a new line. */
    public HTMLPrinter printlnAhref(String dest, String text) {
	printOTag("a", new XMLAttribute[]{ new XMLAttribute("href", dest) });
        print(text);
        return printlnCTag("a");
    }

    /** Prints <A HREF=dest TARGET=target> text </a> tag followed by a new line. */
    public HTMLPrinter printlnAhref(String dest, String target, String text) {
        printOTag("a", new XMLAttribute[]{
            new XMLAttribute("href", dest),
            new XMLAttribute("target", target)});
        print(text);
        return printlnCTag("a");
    }

    /** Prints <A NAME=name> text </a> tag followed by a new line. */
    public HTMLPrinter printlnAname(String anchor, String text) {
        printOTag("a", new XMLAttribute[]{new XMLAttribute("name", anchor)});
        print(text);
        return printlnCTag("a");
    }

    /** Prints text 'text' in bold followed by a new line. */
    public HTMLPrinter printlnBold(String text) {
        printlnTag("b", text);
        return this;
    }

    /** Prints text 'text' in color 'color' followed by a new line. */
    public HTMLPrinter printlnFontColor(String color, String text) {
	printlnTag("font", new XMLAttribute[]{new XMLAttribute("color", color)}, text);
        return this;
    }

    /** Prints comment with contents 'text' followed by a new line. */
    public HTMLPrinter printlnComment(String text) {
	printer.print("<!--");
        printer.print(text);
        printer.println("-->");
        return this;
    }

    /** Prints the <meta> tag with attributes 'attrs' followed by a new line. */
    public HTMLPrinter printlnMeta(XMLAttribute[] attrs) {
	return printlnOTag("meta", attrs);
    }

    /** Prints the <link> tag with attributes 'attrs' followed by a new line. */
    public HTMLPrinter printlnLink(XMLAttribute[] attrs) {
	return printlnOTag("link", attrs);
    }

    //########################################################################
    // Public Methods - Printing simple values

    /** Prints the boolean value. */
    public HTMLPrinter print(boolean value) {
        printer.print(value);
        return this;
    }

    /** Prints the byte value. */
    public HTMLPrinter print(byte value) {
        printer.print(value);
        return this;
    }

    /** Prints the short value. */
    public HTMLPrinter print(short value) {
        printer.print(value);
        return this;
    }

    /** Prints the char value. */
    public HTMLPrinter print(char value) {
        printer.print(value);
        return this;
    }

    /** Prints the int value. */
    public HTMLPrinter print(int value) {
        printer.print(value);
        return this;
    }

    /** Prints the long value. */
    public HTMLPrinter print(long value) {
        printer.print(value);
        return this;
    }

    /** Prints the float value. */
    public HTMLPrinter print(float value) {
        printer.print(value);
        return this;
    }

    /** Prints the long value. */
    public HTMLPrinter print(double value) {
        printer.print(value);
        return this;
    }

    /** Prints the string. */
    public HTMLPrinter print(String value) {
        printer.print(value);
        return this;
    }

    /** Prints the opening HTML tag. */
    public HTMLPrinter printOTag(String label) {
        printer.print("<");
        printer.print(label);
        printer.print(">");
        return this;
    }

    /** Prints the opening HTML tag 'label' with attributes 'attrs'. */
    public HTMLPrinter printOTag(String label, XMLAttribute[] attrs) {
        printer.print("<");
        printer.print(label);
        printer.print(" ");
        printer.print(StringOf.object.array(attrs, "", " ", ""));
        printer.print(">");
        return this;
    }

    /** Prints the closing HTML tag 'label'. */
    public HTMLPrinter printCTag(String label) {
        printer.print("</");
        printer.print(label);
        printer.print(">");
        return this;
    }

    /** Prints the HTML tag 'label' with contents 'text'. */
    public HTMLPrinter printTag(String label, String text) {
        printOTag(label);
        print(text);
        return printCTag(label);
    }

    /** Prints the HTML tag 'label' with attributes 'attrs' and contents 'text'. */
    public HTMLPrinter printTag(String label, XMLAttribute[] attrs, String text) {
        printOTag(label, attrs);
        print(text);
        printCTag(label);
        return this;
    }

    /** Prints the short HTML tag 'label'. */
    public HTMLPrinter printSTag(String label) {
        printer.print("<");
        printer.print(label);
        printer.print("/>");
        return this;
    }

    /** Prints <A HREF=link> text </a> tag. */
    public HTMLPrinter printAhref(String dest, String text) {
        printOTag("a", new XMLAttribute[]{ new XMLAttribute("href", dest) });
        print(text);
        return printCTag("a");
    }

    /** Prints <A HREF=dest TARGET=target> text </a> tag. */
    public HTMLPrinter printAhref(String dest, String target, String text) {
        printOTag("a", new XMLAttribute[]{
            new XMLAttribute("href", dest),
            new XMLAttribute("target", target)});
        print(text);
        return printCTag("a");
    }

    /** Prints <A NAME=name> text </a> tag. */
    public HTMLPrinter printAname(String anchor, String text) {
        printOTag("a", new XMLAttribute[]{ new XMLAttribute("name", anchor) });
        print(text);
        return printCTag("a");
    }

    /**
     * Prints text <code>text</code> in bold.
     * @param text
     */
    public HTMLPrinter printBold(String text) {
        return printTag("b", text);
    }

    /**
     * Prints text <code>text</code> in color <code>color</code>.
     * @param color
     * @param text
     */
    public HTMLPrinter printFontColor(String color, String text) {
	return printTag("font", new XMLAttribute[]{ new XMLAttribute("color", color) }, text);
    }

    /**
     * Prints comment with contents <code>text</code>.
     * @param text
     */
    public HTMLPrinter printComment(String text) {
	printer.print("<!-- ");
        printer.print(text);
        printer.print(" -->");
        return this;
    }

    /**
     * Prints <code>n</code> HTML blank spaces.
     * @param n The parameter <code>n</code> gives the number
     *          of printed blank spaces
     */
    public HTMLPrinter printNbsp(int n) {
	while (n > 0) {
            print("&nbsp;");
            n--;
        }
        return this;
    }

    /**
     * Prints an horizontal line separator.
     */
    public HTMLPrinter printHLine() {
        return printOTag("hr");
    }

    /**
     * Prints an horizontal line separator with attributes <code>attrs</code>.
     * @param attrs
     */
    public HTMLPrinter printHLine(XMLAttribute[] attrs) {
        return printOTag("hr", attrs);
    }

    //########################################################################
    // Public Methods - Converting

    /** Returns the string representation of this printer. */
    public String toString() {
        return printer.toString();
    }

    //########################################################################

    /**
     * Prints the HTML preamble of the current page.
     */
    protected void printPreamble() {
	println("<!DOCTYPE html PUBLIC \"-//W3C//DTD " +
            representation.getType() + "//" + representation.getLanguage() + "\">");
	printlnOTag("html").line();
    }

    /**
     * Prints a comment with generator name and generation date.
     */
    protected void printGeneratedBy(String generator) {
        if (generator != null) {
            SimpleDateFormat df = new SimpleDateFormat("EEE MMM d HH:mm:ss z yyyy");
            printlnComment("Generated by " + generator + " on " +
                df.format(new Date()));
        }
    }

    /**
     * Prints HTML meta informations.
     * @param metaAttrs
     */
    protected void printMetaInfo(XMLAttribute[] metaAttrs) {
        printlnMeta(new XMLAttribute[]{
            new XMLAttribute("http-equiv", "content-type"),
            new XMLAttribute("content",
                             "text/html; charset=" + representation.getEncoding())});
        for (int i = 0; i < metaAttrs.length; i++) {
	    printlnMeta(new XMLAttribute[]{
                new XMLAttribute("name", metaAttrs[i].name),
                new XMLAttribute("content", metaAttrs[i].value)});
        }
    }

    /**
     * Prints HTML link information for style sheets.
     *  @param stylesheets A list of stylesheets to be linked
     *                     to the current HTML document
     */
    protected void printStyles(String[] stylesheets) {
        for (int i = 0; i < stylesheets.length; i++) {
	    printlnLink(new XMLAttribute[]{
                new XMLAttribute("rel", "stylesheet"),
                new XMLAttribute("type", "text/css"),
	        new XMLAttribute("href", stylesheets[i])});
        }
    }

    /**
     * Prints HTML header section of the current page.
     * @param metaAttrs
     * @param generator
     * @param stylesheets
     */
    public void printHeader(XMLAttribute[] metaAttrs, String generator, String[] stylesheets) {
        printPreamble();
        printlnOTag("head").indent();
        printlnTag("title", title);
        printGeneratedBy(generator);
        printMetaInfo(metaAttrs);
        printStyles(stylesheets);
        undent().printlnCTag("head").line();
    }

    /**
     * Prints HTML header section.
     * @param metaXMLAttributes
     * @param generator
     * @param stylesheet
     */
    public void printHeader(XMLAttribute[] metaAttrs, String generator, String stylesheet) {
	printHeader(metaAttrs, generator, new String[]{ stylesheet });
    }

    /**
     * Prints HTML header section.
     * @param metaXMLAttributes
     * @param generator
     */
    public void printHeader(XMLAttribute[] metaAttrs, String generator) {
	printHeader(metaAttrs, generator, DEFAULT_STYLESHEET);
    }

    /**
     * Prints the header section of the current page.
     * @param metaXMLAttributes
     */
    public void printHeader(XMLAttribute[] metaAttrs) {
	printHeader(metaAttrs, null);
    }

    /**
     * Open the body section of the current page.
     */
    public void printOpenBody() {
	printlnOTag("body").indent();
    }

    /**
     * Prints the HTML footer of the current page.
     */
    public void printFootpage() {
	undent().printlnCTag("body");
	printlnCTag("html");
    }

    //########################################################################
}


/**
 * Map from Object to String.
 */
public abstract class StringOf {

    /**
     * Give the string representation of an object.
     * @param o
     */
    public abstract String element(Object o);

    /**
     * Give the string representation of an array of objects.
     * Return delimiters for empty arrays depending on "delimWhenEmpty".
     * @param objects
     * @param open
     * @param sep
     * @param close
     * @param delimWhenEmpty
     */
    public String array(Object[] objects, String open, String sep, String close, boolean delimWhenEmpty) {
	if ((objects.length == 0) && !delimWhenEmpty)
	    return "";
	else {
	    StringBuffer str = new StringBuffer();
	    str.append(open);
	    if (objects.length > 0) {
		str.append(element(objects[0]));
		for(int i = 1; i < objects.length; i++)
		    str.append(sep + element(objects[i]));
	    }
	    str.append(close);
	    return str.toString();
	}
    }

    /**
     * Return always delimiters.
     * @param objects
     * @param open
     * @param sep
     * @param close
     */
    public String array(Object[] objects, String open, String sep, String close) {
	return array(objects, open, sep, close, true);
    }

    /** Basic map.
     */
    public static StringOf object = new StringOf() {
        public String element(Object o) {
            return o.toString();
        }
    };
}
