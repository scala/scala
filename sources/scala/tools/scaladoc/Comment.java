/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.util.*;
import java.util.regex.*;

import ch.epfl.lamp.util.Pair;

import scalac.ast.Tree;
import scalac.symtab.Symbol;

import scaladoc.*;

/**
 * Class <code>Comment</code> contains all information in comment part.
 * It allows users to get first sentence of this comment, get comment
 * for different tags...
 */
public class Comment {

    /**
     * Holder of the comment.
     */
    public final Symbol holder;

    /**
     * Raw text of the comment.
     */
    public final String rawText;

    /**
     * Text minus any tags.
     */
    public String text;

    /**
     * Tags minus text.
     */
    public Tag[] tags;

    /**
     * Constructor.
     *
     * @param holder
     * @param rawText
     */
    public Comment(Symbol holder, String rawText) {
	this.holder = holder;
	this.rawText = cleanComment(rawText);
	parseComment();
    }

    /**
     * Returns true if this comment is empty.
     */
    public boolean isEmpty() {
	return "".equals(rawText);
    }

    /**
     * Removes the leading white spaces and asterixes.
     *
     * @param comment
     */
    protected String cleanComment(String comment) {
	if (comment == null)
	    return "";
	else {
	    comment = comment.substring(3, comment.length() - 2);
	    StringBuffer buff = new StringBuffer();
	    boolean startLine = true;
	    int i = 0;
	    while (i < comment.length()) {
                char ch = comment.charAt(i);
		if (startLine && ((ch == '\t') ||
				  (ch == ' ') ||
				  (ch == '*')))
		    i++;
		else if (startLine)
		    startLine = false;
		else {
		    if ((ch == '\n') || (ch == '\r'))
			startLine = true;
		    buff.append(ch);
		    i++;
		}
	    }
	    return buff.toString();
	}
    }

    /**
     * Parses the comment string separating the description
     * text from tags.
     */
    protected void parseComment() {
	String[] parts = rawText.split("\n@|\\A@");
	if (parts.length == 0) {
	    text = "";
	    tags = new Tag[0];
	}
	else {
	    int startTag;
	    if (parts[0].startsWith("@")) {
		text = "";
		startTag = 0;
	    } else {
		text = parts[0];
		startTag = 1;
	    }
	    List tagList = new LinkedList();
	    for(int i = startTag; i < parts.length; i++) {
		Pair fields = Tag.split(parts[i]);
		String name = (String) fields.fst;
		String description = (String) fields.snd;
		tagList.add(new Tag(holder, "@" + name, description));
	    }
	    tags = (Tag[]) tagList.toArray(new Tag[tagList.size()]);
	}
    }

    /**
     * Returns an array of tags with text and inline.
     *
     * @param holder
     * @param s
     * @see          See Tags for a Doc comment.
     */
    public static Tag[] makeTags(Symbol holder, String s) {
	final List tagList = new LinkedList();
	Pattern p = Pattern.compile("\\{@([^\\}]*)\\}");
	Matcher m = p.matcher(s);

	int start = 0;
	while (m.find()) {
	    String txt = s.substring(start, m.start());
	    if (!txt.equals(""))
		tagList.add(new Tag(holder, "@text", txt));
	    Pair fields = Tag.split(m.group(1));
	    String name = (String) fields.fst;
	    String description = (String) fields.snd;
	    tagList.add(new Tag(holder, "@" + name, description));
	    start = m.end();
	}
	String txt = s.substring(start, s.length());
	if (!txt.equals(""))
	    tagList.add(new Tag(holder, "@text", txt));
	return (Tag[]) tagList.toArray(new Tag[tagList.size()]);
    }

    /**
     * Returns the first sentence of this comment.
     */
    public String firstSentence() {
	Pattern p = Pattern.compile("\\.(\\s)");
	Matcher m = p.matcher(text);
	if (m.find()) {
	    return text.substring(0, m.start(1));;
	} else
	    return text;
    }

}
