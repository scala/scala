/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.{Global => scalac_Global}
import scalac.{Unit => scalac_Unit}
import scalac.PhaseDescriptor;
import scalac.Phase;

package scala.tools.scalac.backend {

/* This class represents the JVMFromICode backend production
 * Phase. It uses the ICode to create class files using
 * the JVM's bytecode */
class GenJVMFromICodePhase(global: scalac_Global, descriptor: PhaseDescriptor) extends Phase(global, descriptor) {

  // ##################################################
  // Public method

  /* Apply this phase to all units */
  def apply(units: Array[scalac_Unit]) = {
    val generator = new GenJVMFromICode(global); // !!! super global
    Iterator.fromArray(units).foreach(generator.translate);
  }
}

}
