/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.util;

import java.util.HashMap;

public class FreshNameCreator {

    protected int counter = 0;
    protected HashMap counters = new HashMap();

    /**
     * Create a fresh name with the given prefix. It is guaranteed
     * that the returned name has never been returned by a previous
     * call to this function with the same separator character (which
     * has to be a non-digit).
     */
    public Name newName(String prefix, char separator) {
        prefix += separator;
        Integer ival = (Integer)counters.get(prefix);
        if (ival == null)
            counters.put(prefix, ival = new Integer(0));
        else
            counters.put(prefix, ival = new Integer(ival.intValue() + 1));
        return Name.fromString(prefix + ival);
    }

    /** Same, with `$' as the separator character
     */
    public Name newName(String prefix) {
        return newName(prefix, '$');
    }

    /** Same, but with a name as prefix. The new name is a type
     *  (respectively, constructor) name if the prefix is one.
     */
    public Name newName(Name prefixName, char separator) {
	Name name = newName(prefixName.toString(), separator);
	if (prefixName.isTypeName()) return name.toTypeName();
	else if (prefixName.isConstrName()) return name.toConstrName();
	else return name;
    }

    /** Same, with `$' as the separator character
     */
    public Name newName(Name prefix) {
        return newName(prefix, '$');
    }

    public Name newName() {
        return Name.fromString("$" + (counter++) + "$");
    }
}
