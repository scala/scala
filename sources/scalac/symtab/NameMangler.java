/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**
** $Id$
\*                                                                      */

package scalac.symtab;

import scalac.util.Name;
import java.util.HashMap;

public class NameMangler {

    private int cnt = 0;

    public void setMangledName(Symbol innerclazz) {
	Symbol topclazz = innerclazz.enclToplevelClass();
	innerclazz.setMangledName(
	    Name.fromString(topclazz.name + "$" + (cnt++) + innerclazz.name));
    }
}
