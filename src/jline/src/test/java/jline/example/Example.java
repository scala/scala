/*
 * Copyright (c) 2002-2006, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline.example;

import jline.*;

import java.io.*;
import java.util.*;
import java.util.zip.*;

public class Example {
    public static void usage() {
        System.out.println("Usage: java " + Example.class.getName()
                + " [none/simple/files/dictionary [trigger mask]]");
        System.out.println("  none - no completors");
        System.out.println("  simple - a simple completor that comples "
                + "\"foo\", \"bar\", and \"baz\"");
        System.out
                .println("  files - a completor that comples " + "file names");
        System.out.println("  dictionary - a completor that comples "
                + "english dictionary words");
        System.out.println("  classes - a completor that comples "
                + "java class names");
        System.out
                .println("  trigger - a special word which causes it to assume "
                        + "the next line is a password");
        System.out.println("  mask - is the character to print in place of "
                + "the actual password character");
        System.out.println("\n  E.g - java Example simple su '*'\n"
                + "will use the simple compleator with 'su' triggering\n"
                + "the use of '*' as a password mask.");
    }

    public static void main(String[] args) throws IOException {
        Character mask = null;
        String trigger = null;

        ConsoleReader reader = new ConsoleReader();
        reader.setBellEnabled(false);
        reader.setDebug(new PrintWriter(new FileWriter("writer.debug", true)));

        if ((args == null) || (args.length == 0)) {
            usage();

            return;
        }

        List<Completor> completors = new LinkedList<Completor>();

        if (args.length > 0) {
            if (args[0].equals("none")) {
            } else if (args[0].equals("files")) {
                completors.add(new FileNameCompletor());
            } else if (args[0].equals("classes")) {
                completors.add(new ClassNameCompletor());
            } else if (args[0].equals("dictionary")) {
                completors.add(new SimpleCompletor(new GZIPInputStream(
                        Example.class.getResourceAsStream("english.gz"))));
            } else if (args[0].equals("simple")) {
                completors.add(new SimpleCompletor(new String[] { "foo", "bar",
                        "baz" }));
            } else {
                usage();

                return;
            }
        }

        if (args.length == 3) {
            mask = new Character(args[2].charAt(0));
            trigger = args[1];
        }

        reader.addCompletor(new ArgumentCompletor(completors));

        String line;
        PrintWriter out = new PrintWriter(System.out);

        while ((line = reader.readLine("prompt> ")) != null) {
            out.println("======>\"" + line + "\"");
            out.flush();

            // If we input the special word then we will mask
            // the next line.
            if ((trigger != null) && (line.compareTo(trigger) == 0)) {
                line = reader.readLine("password> ", mask);
            }
            if (line.equalsIgnoreCase("quit") || line.equalsIgnoreCase("exit")) {
                break;
            }
        }
    }
}
