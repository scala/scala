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
        new meta.scala.MetaFunction(0),
        new meta.scala.MetaFunction(1),
        new meta.scala.MetaFunction(2),
        new meta.scala.MetaFunction(3),
        new meta.scala.MetaFunction(4),
        new meta.scala.MetaFunction(5),
        new meta.scala.MetaFunction(6),
        new meta.scala.MetaFunction(7),
        new meta.scala.MetaFunction(8),
        new meta.scala.MetaFunction(9),
        new meta.scala.MetaTuple(1),
        new meta.scala.MetaTuple(2),
        new meta.scala.MetaTuple(3),
        new meta.scala.MetaTuple(4),
        new meta.scala.MetaTuple(5),
        new meta.scala.MetaTuple(6),
        new meta.scala.MetaTuple(7),
        new meta.scala.MetaTuple(8),
        new meta.scala.MetaTuple(9),
        new meta.scalac.ast.MetaTree(),
        new meta.scalac.ast.MetaTreeFactory(),
        new meta.scalac.ast.MetaDefaultTreeFactory(),
        new meta.scalac.ast.MetaTreeCopier(),
        new meta.scalac.ast.MetaStrictTreeCopier(),
        new meta.scalac.ast.MetaLazyTreeCopier(),
        new meta.scalac.ast.MetaTraverser(),
        new meta.scalac.ast.MetaTransformer(),
        new meta.scalac.checkers.MetaCheckTreeNodes(),
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
                target.delete();
                FileWriter output = new FileWriter(target);
                output.write(writer.toString());
                output.close();
                target.setReadOnly();
            } catch (IOException exception) {
                throw abort(exception);
            }
        }
        if (errors > 0) throw abort();
    }

    //########################################################################
}
