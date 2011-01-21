/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */

package scala.tools.jline.console.history;

import java.util.Iterator;
import java.util.ListIterator;

/**
 * Console history.
 *
 * @author <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 * @author <a href="mailto:jason@planet57.com">Jason Dillon</a>
 * @since 2.3
 */
public interface History
    extends Iterable<History.Entry>
{
    int size();

    boolean isEmpty();

    int index();

    void clear();

    CharSequence get(int index);

    void add(CharSequence line);

    void replace(CharSequence item);

    //
    // Entries
    //

    interface Entry
    {
        int index();

        CharSequence value();
    }

    ListIterator<Entry> entries(int index);

    ListIterator<Entry> entries();

    Iterator<Entry> iterator();

    //
    // Navigation
    //

    CharSequence current();

    boolean previous();

    boolean next();

    boolean moveToFirst();

    boolean moveToLast();

    boolean moveTo(int index);

    void moveToEnd();
}
