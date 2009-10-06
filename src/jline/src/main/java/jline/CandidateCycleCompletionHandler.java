/*
 * Copyright (c) 2002-2007, Marc Prud'hommeaux. All rights reserved.
 *
 * This software is distributable under the BSD license. See the terms of the
 * BSD license in the documentation provided with this software.
 */
package jline;

import java.io.*;
import java.util.*;

/**
 *  <p>
 *  A {@link CompletionHandler} that deals with multiple distinct completions
 *  by cycling through each one every time tab is pressed. This
 *  mimics the behavior of the
 *  <a href="http://packages.qa.debian.org/e/editline.html">editline</a>
 *  library.
 *  </p>
 *  <p><strong>This class is currently a stub; it does nothing</strong></p>
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public class CandidateCycleCompletionHandler implements CompletionHandler {
    public boolean complete(final ConsoleReader reader, final List candidates,
                            final int position) throws IOException {
        throw new IllegalStateException("CandidateCycleCompletionHandler unimplemented");
    }
}
