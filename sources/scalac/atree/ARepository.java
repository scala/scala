/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import java.util.Map;
import java.util.LinkedHashMap;

/** This class represents a repository of attributed classes. */
public class ARepository {

    //########################################################################
    // Private Fields

    /** The symbol to class map */
    private final Map/*<Symbol,AClass>*/ classes;

    //########################################################################
    // Public Constructors

    /** Initializes this instance. */
    public ARepository() {
        this.classes = new LinkedHashMap();
    }

    //########################################################################
    // Public Methods

    /** Adds the given class to this repository. */
    public void addClass(AClass clasz) {
        assert !classes.containsKey(clasz.symbol()): clasz;
        classes.put(clasz.symbol(), clasz);
    }

    /** Returns the classes of this repository. */
    public AClass[] classes() {
        return (AClass[])classes.values().toArray(new AClass[classes.size()]);
    }

    /** Returns a string representation of this repository. */
    public String toString() {
        return new ATreePrinter().printRepository(this).toString();
    }

    //########################################################################
}
