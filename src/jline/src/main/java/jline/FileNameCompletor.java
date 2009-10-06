/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;

import java.io.*;
import java.util.*;

/**
 *  A file name completor takes the buffer and issues a list of
 *  potential completions.
 *
 *  <p>
 *  This completor tries to behave as similar as possible to
 *  <i>bash</i>'s file name completion (using GNU readline)
 *  with the following exceptions:
 *
 *  <ul>
 *  <li>Candidates that are directories will end with "/"</li>
 *  <li>Wildcard regular expressions are not evaluated or replaced</li>
 *  <li>The "~" character can be used to represent the user's home,
 *  but it cannot complete to other users' homes, since java does
 *  not provide any way of determining that easily</li>
 *  </ul>
 *
 *  <p>TODO</p>
 *  <ul>
 *  <li>Handle files with spaces in them</li>
 *  <li>Have an option for file type color highlighting</li>
 *  </ul>
 *
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class FileNameCompletor implements Completor {
    public int complete(final String buf, final int cursor,
                        final List<String> candidates) {
        String buffer = (buf == null) ? "" : buf;

        String translated = buffer;

        // special character: ~ maps to the user's home directory
        if (translated.startsWith("~" + File.separator)) {
            translated = System.getProperty("user.home")
                         + translated.substring(1);
        } else if (translated.startsWith("~")) {
            translated = new File(System.getProperty("user.home")).getParentFile()
                                                                  .getAbsolutePath();
        } else if (!(translated.startsWith(File.separator))) {
            translated = new File("").getAbsolutePath() + File.separator
                         + translated;
        }

        File f = new File(translated);

        final File dir;

        if (translated.endsWith(File.separator)) {
            dir = f;
        } else {
            dir = f.getParentFile();
        }

        final File[] entries = (dir == null) ? new File[0] : dir.listFiles();

        try {
            return matchFiles(buffer, translated, entries, candidates);
        } finally {
            // we want to output a sorted list of files
            sortFileNames(candidates);
        }
    }

    protected void sortFileNames(final List<String> fileNames) {
        Collections.sort(fileNames);
    }

    /**
     *  Match the specified <i>buffer</i> to the array of <i>entries</i>
     *  and enter the matches into the list of <i>candidates</i>. This method
     *  can be overridden in a subclass that wants to do more
     *  sophisticated file name completion.
     *
     *  @param        buffer                the untranslated buffer
     *  @param        translated        the buffer with common characters replaced
     *  @param        entries                the list of files to match
     *  @param        candidates        the list of candidates to populate
     *
     *  @return  the offset of the match
     */
    public int matchFiles(String buffer, String translated, File[] entries,
                          List<String> candidates) {
        if (entries == null) {
            return -1;
        }

        int matches = 0;

        // first pass: just count the matches
        for (int i = 0; i < entries.length; i++) {
            if (entries[i].getAbsolutePath().startsWith(translated)) {
                matches++;
            }
        }

        // green - executable
        // blue - directory
        // red - compressed
        // cyan - symlink
        for (int i = 0; i < entries.length; i++) {
            if (entries[i].getAbsolutePath().startsWith(translated)) {
                String name =
                    entries[i].getName()
                    + (((matches == 1) && entries[i].isDirectory())
                       ? File.separator : " ");

                /*
                if (entries [i].isDirectory ())
                {
                        name = new ANSIBuffer ().blue (name).toString ();
                }
                */
                candidates.add(name);
            }
        }

        final int index = buffer.lastIndexOf(File.separator);

        return index + File.separator.length();
    }
}
