/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package ch.epfl.lamp.util;

/**
 * Pairs of values.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class Pair {
    public final Object fst, snd;
    public Pair(Object fst, Object snd) {
        this.fst = fst;
        this.snd = snd;
    }
}
