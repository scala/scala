/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;

import java.io.*;
import java.text.MessageFormat;
import java.util.*;

/**
 *  <p>
 *  A {@link CompletionHandler} that deals with multiple distinct completions
 *  by outputting the complete list of possibilities to the console. This
 *  mimics the behavior of the
 *  <a href="http://www.gnu.org/directory/readline.html">readline</a>
 *  library.
 *  </p>
 *
 *  <strong>TODO:</strong>
 *  <ul>
 *        <li>handle quotes and escaped quotes</li>
 *        <li>enable automatic escaping of whitespace</li>
 *  </ul>
 *
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class CandidateListCompletionHandler implements CompletionHandler {
    private static ResourceBundle loc = ResourceBundle.
        getBundle(CandidateListCompletionHandler.class.getName());

    private boolean eagerNewlines = true;

    public void setAlwaysIncludeNewline(boolean eagerNewlines) {
        this.eagerNewlines = eagerNewlines;
    }

    public boolean complete(final ConsoleReader reader, final List<String> candidates,
                            final int pos) throws IOException {
        CursorBuffer buf = reader.getCursorBuffer();

        // if there is only one completion, then fill in the buffer
        if (candidates.size() == 1) {
            String value = candidates.get(0).toString();

            // fail if the only candidate is the same as the current buffer
            if (value.equals(buf.toString())) {
                return false;
            }

            setBuffer(reader, value, pos);

            return true;
        } else if (candidates.size() > 1) {
            String value = getUnambiguousCompletions(candidates);
            String bufString = buf.toString();
            setBuffer(reader, value, pos);
        }

        if (eagerNewlines)
            reader.printNewline();
        printCandidates(reader, candidates, eagerNewlines);

        // redraw the current console buffer
        reader.drawLine();

        return true;
    }

    public static void setBuffer(ConsoleReader reader, String value, int offset)
                           throws IOException {
        while ((reader.getCursorBuffer().cursor > offset)
                   && reader.backspace()) {
            ;
        }

        reader.putString(value);
        reader.setCursorPosition(offset + value.length());
    }

    /**
     *  Print out the candidates. If the size of the candidates
     *  is greated than the {@link getAutoprintThreshhold},
     *  they prompt with aq warning.
     *
     *  @param  candidates  the list of candidates to print
     */
    public static final void printCandidates(ConsoleReader reader,
                                             Collection<String> candidates, boolean eagerNewlines)
                                throws IOException {
        Set<String> distinct = new HashSet<String>(candidates);

        if (distinct.size() > reader.getAutoprintThreshhold()) {
            if (!eagerNewlines)
                reader.printNewline();
            reader.printString(MessageFormat.format
                (loc.getString("display-candidates"), new Object[] {
                    new Integer(candidates .size())
                    }) + " ");

            reader.flushConsole();

            int c;

            String noOpt = loc.getString("display-candidates-no");
            String yesOpt = loc.getString("display-candidates-yes");

            while ((c = reader.readCharacter(new char[] {
                yesOpt.charAt(0), noOpt.charAt(0) })) != -1) {
                if (noOpt.startsWith
                    (new String(new char[] { (char) c }))) {
                    reader.printNewline();
                    return;
                } else if (yesOpt.startsWith
                    (new String(new char[] { (char) c }))) {
                    break;
                } else {
                    reader.beep();
                }
            }
        }

        // copy the values and make them distinct, without otherwise
        // affecting the ordering. Only do it if the sizes differ.
        if (distinct.size() != candidates.size()) {
            Collection<String> copy = new ArrayList<String>();

            for (Iterator<String> i = candidates.iterator(); i.hasNext();) {
                String next = i.next();

                if (!(copy.contains(next))) {
                    copy.add(next);
                }
            }

            candidates = copy;
        }

        reader.printNewline();
        reader.printColumns(candidates);
    }

    /**
     *  Returns a root that matches all the {@link String} elements
     *  of the specified {@link List}, or null if there are
     *  no commalities. For example, if the list contains
     *  <i>foobar</i>, <i>foobaz</i>, <i>foobuz</i>, the
     *  method will return <i>foob</i>.
     */
    private final String getUnambiguousCompletions(final List<String> candidates) {
        if ((candidates == null) || (candidates.size() == 0)) {
            return null;
        }

        // convert to an array for speed
        String[] strings =
            (String[]) candidates.toArray(new String[candidates.size()]);

        String first = strings[0];
        StringBuffer candidate = new StringBuffer();

        for (int i = 0; i < first.length(); i++) {
            if (startsWith(first.substring(0, i + 1), strings)) {
                candidate.append(first.charAt(i));
            } else {
                break;
            }
        }

        return candidate.toString();
    }

    /**
     *  @return  true is all the elements of <i>candidates</i>
     *                          start with <i>starts</i>
     */
    private final boolean startsWith(final String starts,
                                     final String[] candidates) {
        for (int i = 0; i < candidates.length; i++) {
            if (!candidates[i].startsWith(starts)) {
                return false;
            }
        }

        return true;
    }
}
