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
 *  Handler for dealing with candidates for tab-completion.
 *
 *  @author  <a href="mailto:mwp1@cornell.edu">Marc Prud'hommeaux</a>
 */
public interface CompletionHandler {
    boolean complete(ConsoleReader reader, List<String> candidates, int position)
              throws IOException;
}
