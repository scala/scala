/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/** This class implements an empty iterator. */
public class EmptyIterator implements Iterator {

    //########################################################################
    // Public Constants

    /** The unique instance of this class. */
    public static final EmptyIterator object = new EmptyIterator();

    //########################################################################
    // Private Constructors

    /** Initializes this instance. */
    private EmptyIterator() {}

    //########################################################################
    // Public Methods

    /** Returns false. */
    public boolean hasNext() {
        return false;
    }

    /** Throws NoSuchElementException. */
    public Object next() {
        throw new NoSuchElementException();
    }

    /** Throws IllegalStateException. */
    public void remove() {
        throw new IllegalStateException();
    }

    //########################################################################
}
