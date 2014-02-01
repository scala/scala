/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Jason Zaugg
 */

package scala.tools.nsc
package backend.jvm
import scala.tools.nsc.symtab._

object GenCommon {
  val ExcludedForwarderFlags = {
    import Flags._
    // Should include DEFERRED but this breaks findMember.
    (SPECIALIZED | LIFTED | PROTECTED | STATIC | EXPANDEDNAME | BridgeAndPrivateFlags | MACRO)
  }
}
