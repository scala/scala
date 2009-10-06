/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;

import java.util.*;

/**
 *  <p>
 *  A completor that contains multiple embedded completors. This differs
 *  from the {@link ArgumentCompletor}, in that the nested completors
 *  are dispatched individually, rather than delimited by arguments.
 *  </p>
 *
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class MultiCompletor implements Completor {
    Completor[] completors = new Completor[0];

    /**
     *  Construct a MultiCompletor with no embedded completors.
     */
    public MultiCompletor() {
        this(new Completor[0]);
    }

    /**
     *  Construct a MultiCompletor with the specified list of
     *  {@link Completor} instances.
     */
    public MultiCompletor(final List<Completor> completors) {
        this((Completor[]) completors.toArray(new Completor[completors.size()]));
    }

    /**
     *  Construct a MultiCompletor with the specified
     *  {@link Completor} instances.
     */
    public MultiCompletor(final Completor[] completors) {
        this.completors = completors;
    }

    public int complete(final String buffer, final int pos, final List<String> cand) {
        int[] positions = new int[completors.length];
        List<List<String>> copies = new ArrayList<List<String>>();
        for (int i = 0; i < completors.length; i++) {
            copies.add(null);
        }

        for (int i = 0; i < completors.length; i++) {
            // clone and save the candidate list
            copies.set(i, new LinkedList<String>(cand));
            positions[i] = completors[i].complete(buffer, pos, copies.get(i));
        }

        int maxposition = -1;

        for (int i = 0; i < positions.length; i++) {
            maxposition = Math.max(maxposition, positions[i]);
        }

        // now we have the max cursor value: build up all the
        // candidate lists that have the same cursor value
        for (int i = 0; i < copies.size(); i++) {
            if (positions[i] == maxposition) {
                cand.addAll(copies.get(i));
            }
        }

        return maxposition;
    }

    public void setCompletors(final Completor[] completors) {
        this.completors = completors;
    }

    public Completor[] getCompletors() {
        return this.completors;
    }
}
