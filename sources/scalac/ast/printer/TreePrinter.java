/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.ast.printer;

import java.io.OutputStream;

import scalac.Unit;
import scalac.ast.Tree;

/**
 * Interface for all abstract tree printers.
 *
 * @author Michel Schinz
 * @version 1.0
 */
public interface TreePrinter {
    public void begin();
    public void end();
    public void flush();

    public void beginSection(int level, String title);

    public void print(Unit unit);

    public TreePrinter print(Tree tree);
    public TreePrinter print(String str);
    public TreePrinter println();
}
