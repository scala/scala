/*     ___ ____ ___   __   ___  _____
**    / _// __// _ | / /  / _ |/_  _/     Scala test
**  __\ \/ /__/ __ |/ /__/ __ | / /       (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_//_/
**
**  $Id$
*/

package scala.tools.scalatest;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;


public abstract class Command {

    private Console console;

    Command(Console console) {
        this.console = console;
    }

    /**
     * Redirects the standard output/error streams of the process into files.
     * (see http://www.javaworld.com/javaworld/jw-12-2000/jw-1229-traps_p.html)
     */
    private class StreamGobbler extends Thread {
        private InputStream is;
        private OutputStream os;

        public StreamGobbler(InputStream is) {
            // this(is, null);  // Pico bug (mics/14.10.2003)
            this.is = is;
            this.os = null;
        }

        public StreamGobbler(InputStream is, OutputStream os) {
            this.is = is;
            this.os = os;
        }

        public void run() {
            try {
                BufferedReader rd =
                    new BufferedReader(new InputStreamReader(is));
                String s = rd.readLine();
                if (s != null) {
                    if (os == null)
                        do {
                            console.println(s);
                        } while ((s = rd.readLine()) != null);
                    else {
                        PrintWriter pw = new PrintWriter(os);
                        do {
                            pw.println(s);
                        } while ((s = rd.readLine()) != null);
                        pw.flush();
                    }
                }
            } catch (Exception e) {
                System.err.println(e.getMessage());
                System.exit(-1);
            }
        }
    }

    protected boolean execute(String cmdline, OutputStream out, OutputStream err) {
        boolean ok = true;
        try {
            Process p = Runtime.getRuntime().exec(cmdline);
            StreamGobbler outputGobbler = new StreamGobbler(p.getInputStream(), out);
            StreamGobbler errorGobbler = new StreamGobbler(p.getErrorStream(), err);

            // read any output from the attempted command
            outputGobbler.start();

            // read any errors from the attempted command
            errorGobbler.start();

            int exitValue = p.waitFor();
            ok = exitValue >= 0;

            if (out != null)
                out.flush();
            if (err != null)
                err.flush();
        } catch (Exception e) {
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
