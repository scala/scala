/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.{Global => scalac_Global}
import scalac.CompilationUnit;
import scalac.PhaseDescriptor;
import scalac.Phase;

package scala.tools.scalac.backend {

/* This class represents the JVMFromICode backend production
 * Phase. It uses the ICode to create class files using
 * the JVM's bytecode */
class GenJVMFromICodePhase(global0: scalac_Global, descriptor0: PhaseDescriptor) extends Phase(global0, descriptor0) {

  //##########################################################################
  // Private Fields

  private val generator = new GenJVMFromICode(global);

  //##########################################################################
  // Public Methods

  /** Applies this phase to the given compilation unit. */
  override def apply(unit: CompilationUnit): Unit = generator.translate(unit);

  //##########################################################################
}
}
