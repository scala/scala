/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.util.regex.*;

import ch.epfl.lamp.util.Pair;

import scalac.ast.Tree;
import scalac.symtab.Symbol;

import scaladoc.*;

/**
 * Documentation tag.
 */
public class Tag {

    /**
     * Symbol whose comment contains the tag.
     */
    public final Symbol holder;

    /**
     * Name of the tag.
     */
    public final String name;

    /**
     * Value of the tag.
     */
    public final String text;

    /**
     * Constructor
     *
     * @param name
     * @param text
     */
    public Tag(Symbol holder, String name, String text) {
        this.holder = holder;
        this.name = name;
        this.text = text;
    }

    /** String representation.
     */
    public String toString() {
	return "Tag(" + name + ", " + text + ")";
    }

    /**
     * Is this tag a param tag ?
     */
    public boolean isParam() {
	return name.equals("@param");
    }

    /**
     * Is this tag an exception tag ?
     */
    public boolean isException() {
	return name.equals("@param") || name.equals("@exception");
    }

    /**
     * Is this tag a reference tag ?
     */
    public boolean isReference() {
	return name.equals("@see") || name.equals("@link");
    }

    /**
     * Is this tag a text tag ?
     */
    public boolean isText() {
	return name.equals("@text");
    }

    /**
     * Splits a string containing spaces into two parts.
     *
     * @param text
     */
    public static Pair split(String text) {
	int length = text.length();
	int endFst = length;
	int startSnd = length;
	int current = 0;
	while (current < length && startSnd >= length) {
	    char c = text.charAt(current);
	    if (Character.isWhitespace(c) && endFst == length)
		endFst = current;
	    else if (!Character.isWhitespace(c) && endFst != length)
		startSnd = current;
	    current++;
	}
	return new Pair(text.substring(0, endFst),
			text.substring(startSnd, length));
    }

    /**
     * Kind of a reference tag.
     */
    public static class RefKind {
	/** Bad reference. */
	public case Bad(String ref);

	/** Reference to an URL. */
	public case Url(String ref);

	/** String literal reference. */
	public case Literal(String ref);

	/** Reference to a scala entity. */
	public case Scala(String container, String member, String label);
    }

    /**
     * Parses a reference tag.
     *
     * @param tag
     */
    protected static RefKind parseReference(Tag tag) {
	if (tag.text.startsWith("\""))
	    return RefKind.Literal(tag.text);
	else if (tag.text.startsWith("<"))
	    return RefKind.Url(tag.text);
	else {
	    Pattern p = Pattern.compile("(([^#\\s]*)(#([^\\s]+))?)\\s*(.+)?");
	    Matcher m = p.matcher(tag.text);
	    if (m.find()) {
		String container = m.group(2) == null ? "" : m.group(2);
		String member = m.group(3);
		String label =  m.group(4) == null ? "" : m.group(4);
		return RefKind.Scala(container, member, label);
	    } else {
		System.err.println("Warning: Fail to parse see tag:" + tag + " in " + tag.holder);
		return RefKind.Bad(tag.text);
	    }
	}
    }

}
