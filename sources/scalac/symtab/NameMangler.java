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

    private HashMap/*<Symbol,HashMap<Symbol,int[]>>*/ mangleMap = new HashMap();

    public void setMangledName(Symbol innerclazz) {
	Symbol topclazz = innerclazz.enclToplevelClass();
	HashMap map = (HashMap) mangleMap.get(topclazz);
	if (map == null) {
	    map = new HashMap();
	    mangleMap.put(topclazz, map);
	}
	int[] ctr = (int[]) map.get(innerclazz);
	if (ctr == null) {
	    ctr = new int[1];
	    map.put(innerclazz, ctr);
	}
	innerclazz.setMangledName(
	    Name.fromString(topclazz.name + "$" + (ctr[0]++) + innerclazz.name));
    }
}
