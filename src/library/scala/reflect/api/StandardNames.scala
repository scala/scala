/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

trait StandardNames { self: Universe =>

  val nme: AbsTermNames

  abstract class AbsTermNames {
    val CONSTRUCTOR: TermName
  }

  val tpnme: AbsTypeNames

  abstract class AbsTypeNames {
  }
}
