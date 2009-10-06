/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;

import java.util.*;

/**
 *  A Completor is the mechanism by which tab-completion candidates
 *  will be resolved.
 *
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public interface Completor {
    /**
     *  Populates <i>candidates</i> with a list of possible
     *  completions for the <i>buffer</i>. The <i>candidates</i>
     *  list will not be sorted before being displayed to the
     *  user: thus, the complete method should sort the
     *  {@link List} before returning.
     *
     *
     *  @param  buffer     the buffer
     *  @param  candidates the {@link List} of candidates to populate
     *  @return            the index of the <i>buffer</i> for which
     *                     the completion will be relative
     */
    int complete(String buffer, int cursor, List<String> candidates);
}
