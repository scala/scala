/* NSC -- new Scala compiler
 * Copyright 2005-2014 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package backend.jvm

import scala.tools.asm
import asm.tree._

/**
 * Reporting utilities used in the optimizer.
 */
object OptimizerReporting {
  def methodSignature(className: String, methodName: String, methodDescriptor: String): String = {
    className + "::" + methodName + methodDescriptor
  }

  def methodSignature(className: String, method: MethodNode): String = methodSignature(className, method.name, method.desc)

  def inlineFailure(reason: String): Nothing = MissingRequirementError.signal(reason)
  def assertionError(message: String): Nothing = throw new AssertionError(message)
}
