/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package meta;

import java.io.File;
import java.io.FileWriter;
import java.io.BufferedWriter;
import java.io.PrintWriter;
import java.io.PrintStream;
import java.io.IOException;

import meta.util.AbstractMain;
import meta.util.AbstractFileExpander;
import meta.util.TextExpander;
import meta.util.TextWriter;

/** A program that invokes all file generators. */
public class GenerateAll extends AbstractMain {

    //########################################################################
    // Public Constants

    public static final AbstractFileExpander[] expanders = {
        new meta.scalac.ast.MetaTreeFactory(),
        new meta.scalac.ast.MetaDefaultTreeFactory(),
    };

    //########################################################################
    // Public Functions

    public static void usage(PrintStream out) {
        out.println("usage: " + script() + " <outputdir> [<logfile>]");
    }

    public static void main(String[] args) throws Exception {
        if (args.length < 1 || 2 < args.length) {
            usage(System.err);
            throw abort();
        }
        int errors = 0;
        File root = new File(args[0]);
        PrintWriter filelist = args.length <= 1 ? null :
            new PrintWriter(new BufferedWriter(new FileWriter(args[1])));
        for (int i = 0; i < expanders.length; i++) {
            File source = expanders[i].getSourceFile(root);
            File target = expanders[i].getTargetFile(root);
            System.out.println("Generating file " + target);
            if (filelist != null) {filelist.println(target);filelist.flush();}
            try {
                TextWriter writer = expanders[i].getTextWriter();
                TextExpander expander = new TextExpander(writer, expanders[i]);
                expander.expandText(source);
                errors += expander.getErrorCount();
                FileWriter output = new FileWriter(target);
                output.write(writer.toString());
                output.close();
            } catch (IOException exception) {
                throw abort(exception);
            }
        }
        if (errors > 0) throw abort();
    }

    //########################################################################
}
