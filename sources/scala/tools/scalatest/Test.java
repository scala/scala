/*     ___ ____ ___   __   ___  _____
**    / _// __// _ | / /  / _ |/_  _/     Scala test
**  __\ \/ /__/ __ |/ /__/ __ | / /       (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_//_/
**
**  $Id$
*/

package scala.tools.scalatest;

import java.util.List;
import java.util.ListIterator;


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
