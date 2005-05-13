/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.util.Position;

/** The main attribution phase.
 */
abstract class Analyzer
	 extends Contexts
	 with Namers
	 with Typers
	 with Infer
	 with Variances
         with EtaExpansion {
  val global: Global;
}
