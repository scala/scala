/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scala.tools.scaladoc;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import scalac.Global;
import scalac.Unit;
import scalac.symtab.Symbol;
import scalac.util.Debug;

/**
 * Phase which generates HTML documentation.
 */
public class StandardDocModule {

    private final Global global;

    /**
     * Constructor
     *
     * @param globlal
     */
    public StandardDocModule(Global global) {
        super();
	this.global = global;
    }

    /**
     * ..
     */
    public void apply() {
        HTMLGenerator.apply(global);
    }

    /**
     * ..
     *
     * @param global
     */
    public static void apply(Global global) {
	new StandardDocModule(global).apply();
    }

}
