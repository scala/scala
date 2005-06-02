/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

/** The main attribution phase.
 */
abstract class Analyzer
	 extends SubComponent
            with Contexts
    	    with Namers
	    with Typers
	    with Infer
	    with Variances
            with EtaExpansion {
  val global: Global;
}

