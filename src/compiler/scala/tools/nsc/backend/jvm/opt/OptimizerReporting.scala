/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm

import scala.tools.asm
import asm.tree._
import scala.tools.nsc.backend.jvm.BTypes.InternalName

/**
 * Reporting utilities used in the optimizer.
 *
 * TODO: move out of opt package, rename: it's already used outside the optimizer.
 * Centralize backend reporting here.
 */
object OptimizerReporting {
  def methodSignature(classInternalName: InternalName, method: MethodNode): String = {
    classInternalName + "::" + method.name + method.desc
  }

  def inlineFailure(reason: String): Nothing = MissingRequirementError.signal(reason)
  def assertionError(message: String): Nothing = throw new AssertionError(message)
}
