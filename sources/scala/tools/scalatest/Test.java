/*     ___ ____ ___   __   ___  _____
**    / _// __// _ | / /  / _ |/_  _/     Scala test
**  __\ \/ /__/ __ |/ /__/ __ | / /       (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_//_/
**
**  $Id$
*/

package scala.tools.scalatest;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.List;
import java.util.ListIterator;


abstract class Command {
    private Console console;

    Command(Console console) {
        this.console = console;
    }

    private void redirect(BufferedReader rd, OutputStream out) throws IOException {
        String s = rd.readLine();
        if (s != null) {
            if (out == null) /* no redirection */
                do {
                    console.println(s);
                } while ((s = rd.readLine()) != null);
            else {
                PrintWriter pw = new PrintWriter(out);
                do {
                    pw.println(s);
                } while ((s = rd.readLine()) != null);
                pw.close();
            }
        }
    }

    protected boolean execute(String cmdline, OutputStream out, OutputStream err) {
        boolean ok = true;
        try {
            // see http://www.devdaily.com/java/edu/pj/pj010016/pj010016.shtml
            Process p = Runtime.getRuntime().exec(cmdline);
            BufferedReader stdOutput = new BufferedReader(
                new InputStreamReader(p.getInputStream()));
            BufferedReader stdError = new BufferedReader(
                new InputStreamReader(p.getErrorStream()));

            // read any output from the attempted command
            redirect(stdOutput, out);

            // read any errors from the attempted command
            redirect(stdError, err);

            p.destroy();
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
        return ok;
    }

    protected boolean execute(String cmdline, OutputStream out) {
        return execute(cmdline, out, null);
    }

    protected boolean execute(String cmdline) {
        return execute(cmdline, null, null);
    }

    public abstract boolean run(String arg);

}


public abstract class Test {
    private List/*String*/[] groups;

    protected static Console console;
    protected String description;
    protected int filesCount;

    Test(String description, List[] groups) {
        this.description = description;
        this.groups = groups;
        for (int i = 0; i < groups.length; i++)
            filesCount += groups[i].size();
    }

    Test(String description, List group) {
        this(description, new List[]{ group });
    }

    protected int run(Command cmd) {
        int successCount = 0;
        if (filesCount > 0) {
            console.println();
            console.println(description);
            for (int i = 0; i < groups.length; i++)
                for (ListIterator it = groups[i].listIterator(); it.hasNext();) {
                    boolean success = cmd.run((String) it.next());
                    if (success) ++successCount;
                }
        }
        return successCount;
    }

    public abstract int run();

    public static void setConsole(Console con) {
       console = con;
    }

}
