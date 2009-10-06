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
 *  <p>
 *  A pass-through application that sets the system input stream to a
 *  {@link ConsoleReader} and invokes the specified main method.
 *  </p>
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class ConsoleRunner {
    public static final String property = "jline.history";

    public static void main(final String[] args) throws Exception {
        String historyFileName = null;

        List<String> argList = new ArrayList<String>(Arrays.asList(args));

        if (argList.size() == 0) {
            usage();

            return;
        }

        historyFileName = System.getProperty(ConsoleRunner.property, null);

        // invoke the main() method
        String mainClass = (String) argList.remove(0);

        // setup the inpout stream
        ConsoleReader reader = new ConsoleReader();

        if (historyFileName != null) {
            reader.setHistory(new History (new File
                (System.getProperty("user.home"),
                    ".jline-" + mainClass
                        + "." + historyFileName + ".history")));
        } else {
            reader.setHistory(new History(new File
                (System.getProperty("user.home"),
                    ".jline-" + mainClass + ".history")));
        }

        String completors = System.getProperty
            (ConsoleRunner.class.getName() + ".completors", "");
        List<Completor> completorList = new ArrayList<Completor>();

        for (StringTokenizer tok = new StringTokenizer(completors, ",");
            tok.hasMoreTokens();) {
            completorList.add
                ((Completor) Class.forName(tok.nextToken()).newInstance());
        }

        if (completorList.size() > 0) {
            reader.addCompletor(new ArgumentCompletor(completorList));
        }

        ConsoleReaderInputStream.setIn(reader);

        try {
            Class.forName(mainClass).
                getMethod("main", new Class[] { String[].class }).
                invoke(null, new Object[] { argList.toArray(new String[0]) });
        } finally {
            // just in case this main method is called from another program
            ConsoleReaderInputStream.restoreIn();
        }
    }

    private static void usage() {
        System.out.println("Usage: \n   java " + "[-Djline.history='name'] "
            + ConsoleRunner.class.getName()
            + " <target class name> [args]"
            + "\n\nThe -Djline.history option will avoid history"
            + "\nmangling when running ConsoleRunner on the same application."
            + "\n\nargs will be passed directly to the target class name.");
    }
}
