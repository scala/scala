/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.Date;

/** This class provides methods to print HTML document.
 */
public class HTMLPrinter {

    //########################################################################
    // Private Fields

    /** The underlying code printer */
    private final CodePrinter printer;

    /** The document title */
    private final String title;

    /** The document type */
    private final String type;

    /** The document character encoding */
    private final String encoding;

    //########################################################################
    // Public Constructors

    /** Creates a new instance */
    public HTMLPrinter(Writer writer, String title, String type, String encoding) {
        this.printer = new CodePrinter(writer, "  ");
        this.title = title;
        this.type = type;
        this.encoding = encoding;
    }

    /** Creates a new instance */
    public HTMLPrinter(Writer writer, String title, String encoding) {
        this.printer = new CodePrinter(writer, "  ");
        this.title = title;
        this.type = "HTML 4.01 Transitional";
        this.encoding = encoding;
    }

    /** Creates a new instance */
    public HTMLPrinter(Writer writer, String title) {
        this(writer, title, "iso-8859-1");
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

    /** Returns the underlying document type. */
    public String getType() {
        return type;
    }

    /** Returns the underlying character encoding. */
    public String getEncoding() {
        return encoding;
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
    // Public Methods - Printing simple values

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
    public HTMLPrinter printlnOTag(String label, Attr[] attrs) {
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
        printlnCTag(label);
        return this;
    }

    /** Prints the HTML tag 'label' with attributes 'attrs' and contents 'text'
     *  followed by a new line.
     */
    public HTMLPrinter printlnTag(String label, Attr[] attrs, String text) {
        printOTag(label, attrs);
        print(text);
        printlnCTag(label);
        return this;
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
    public HTMLPrinter printlnSTag(String label, Attr[] attrs) {
        printer.print("<");
        printer.print(label);
        printer.print(" ");
        printer.print(StringOf.object.array(attrs, "", " ", ""));
        printer.println("/>");
        return this;
    }

    /** Prints <A HREF=link> text </a> tag followed by a new line. */
    public HTMLPrinter printlnAhref(String dest, String text) {
	printOTag("a", new Attr[]{ new Attr("href", dest) });
        print(text);
        printlnCTag("a");
        return this;
    }

    /** Prints <A HREF=dest TARGET=target> text </a> tag followed by a new line. */
    public HTMLPrinter printlnAhref(String dest, String target, String text) {
        printOTag("a", new Attr[]{
            new Attr("href", dest),
            new Attr("target", target)});
        print(text);
        printlnCTag("a");
        return this;
    }

    /** Prints <A NAME=name> text </a> tag followed by a new line. */
    public HTMLPrinter printlnAname(String anchor, String text) {
        printOTag("a", new Attr[]{new Attr("name", anchor)});
        print(text);
        printlnCTag("a");
        return this;
    }

    /** Prints text 'text' in bold followed by a new line. */
    public HTMLPrinter printlnBold(String text) {
        printlnTag("b", text);
        return this;
    }

    /** Prints text 'text' in color 'color' followed by a new line. */
    public HTMLPrinter printlnFontColor(String color, String text) {
	printlnTag("font", new Attr[]{new Attr("color", color)}, text);
        return this;
    }

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
    public HTMLPrinter printOTag(String label, Attr[] attrs) {
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
        printCTag(label);
        return this;
    }

    /** Prints the HTML tag 'label' with attributes 'attrs' and contents 'text'. */
    public HTMLPrinter printTag(String label, Attr[] attrs, String text) {
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
        printOTag("a", new Attr[]{ new Attr("href", dest) });
        print(text);
        printCTag("a");
        return this;
    }

    /** Prints <A HREF=dest TARGET=target> text </a> tag. */
    public HTMLPrinter printAhref(String dest, String target, String text) {
        printOTag("a", new Attr[]{
            new Attr("href", dest),
            new Attr("target", target)});
        print(text);
        printCTag("a");
        return this;
    }

    /** Prints <A NAME=name> text </a> tag. */
    public HTMLPrinter printAname(String anchor, String text) {
        printOTag("a", new Attr[]{ new Attr("name", anchor) });
        print(text);
        printCTag("a");
        return this;
    }

    /** Prints text 'text' in bold. */
    public HTMLPrinter printBold(String text) {
        printTag("b", text);
        return this;
    }

    /** Prints text 'text' in color 'color'. */
    public HTMLPrinter printFontColor(String color, String text) {
	printTag("font", new Attr[]{ new Attr("color", color) }, text);
        return this;
    }

    /** Prints n HTML blank spaces.
     *  @ param n gives the number of printed blank spaces
     */
    public HTMLPrinter printNbsp(int n) {
	while (n > 0) {
            print("&nbsp;");
            n--;
        }
        return this;
    }

    /** Prints an horizontal line separator */
    public HTMLPrinter printHLine() {
        printOTag("hr");
        return this;
    }

    /** Prints an horizontal line separator with attributes 'attrs'. */
    public HTMLPrinter printHLine(Attr[] attrs) {
        printOTag("hr", attrs);
        return this;
    }

    //########################################################################
    // Public Methods - Converting

    /** Returns the string representation of this printer. */
    public String toString() {
        return printer.toString();
    }

    //########################################################################

    /** Prints HTML preamble.
     */
    protected void printPreamble() {
	println("<!DOCTYPE html PUBLIC \"-//W3C//DTD " + type + "//EN\">");
	printlnOTag("html").line();
    }

    /**
     */
    protected void printGeneratedBy(String name) {
        SimpleDateFormat df = new SimpleDateFormat("EEE MMM d HH:mm:ss z yyyy");
        println("<!-- Generated by " + name + " on " +
            df.format(new Date()) + " -->");
    }

    /** Prints HTML meta information.
     */
    protected void printMetaInfo(String name) {
	printlnOTag("meta", new Attr[]{
            new Attr("http-equiv", "content-type"),
            new Attr("content", "text/html; charset=" + encoding)});
	printlnOTag("meta", new Attr[]{
            new Attr("name", "author"),
            new Attr("content", name)});
    }

    protected void printStyle() {
	//printlnOTag("link", new Attr[]{ new Attr("rel", "stylesheet"),
	//				    new Attr("href", "style.css")});
	println("<style type=\"text/css\">");
	println("<!--").indent();
	println("a:link { color: #0000ee; }");
	println("a:visited { color: #551a8b; }");
	println("a:active { color: #0000ee; }");
	println("body { background-color: #ffffff; }").undent();
	println("-->");
	println("</style>");
    }

    /** Prints HTML header section.
     * @param name
     */
    public void printHeader(String name) {
	printPreamble();
	printlnOTag("head").indent();
        printlnTag("title", title);
        printGeneratedBy(name);
        printMetaInfo(name);
        printStyle();
	undent().printlnCTag("head").line();
    }

    /** Open the body section.
     */
    public void printOpenBody() {
	printlnOTag("body").indent();
    }

    /**
     */
    public void printFootpage() {
	undent().printlnCTag("body");
	printlnCTag("html");
    }

    //########################################################################
}


/** Map from Object to String.
 */
public abstract class StringOf {

    /** Give the string representation of an object.
     */
    public abstract String element(Object o);

    /** Give the string representation of an array of objects. Return delimiters
     *  for empty arrays depending on "delimWhenEmpty".
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

    /** Return always delimiters.
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
