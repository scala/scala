/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

package scalac.util;

import java.util.*;

/**
 * Class to assign unique and small numbers to objects, based on their
 * identity.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class UniqueID {
    protected Map ids = new HashMap();

    public int id(Object obj) {
        if (! ids.containsKey(obj))
            ids.put(obj, new Integer(ids.size()));
        return ((Integer)ids.get(obj)).intValue();
    }
}
