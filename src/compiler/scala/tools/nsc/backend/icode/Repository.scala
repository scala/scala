/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */


package scala.tools.nsc
package backend
package icode

import scala.collection._

/**
 *  @author Iulian Dragos
 */
trait Repository {
  val global: Global
  import global._
  import icodes._

  val loaded: mutable.Map[Symbol, IClass] = perRunCaches.newMap()

  /** Is the given class available as icode? */
  def available(sym: Symbol) = classes.contains(sym) || loaded.contains(sym)

  /** The icode of the given class, if available */
  def icode(sym: Symbol): Option[IClass] = (classes get sym) orElse (loaded get sym)

  /** The icode of the given class. If not available, it loads
   *  its bytecode.
   */
  def icode(sym: Symbol, force: Boolean): IClass =
    icode(sym) getOrElse {
      log("loading " + sym)
      load(sym)
      assert(available(sym))
      loaded(sym)
    }

  /** Load bytecode for given symbol. */
  def load(sym: Symbol): Boolean = {
    try {
      val (c1, c2) = icodeReader.readClass(sym)

      assert(c1.symbol == sym || c2.symbol == sym, "c1.symbol = %s, c2.symbol = %s, sym = %s".format(c1.symbol, c2.symbol, sym))
      loaded += (c1.symbol -> c1)
      loaded += (c2.symbol -> c2)

      true
    } catch {
      case e: Throwable => // possible exceptions are MissingRequirementError, IOException and TypeError -> no better common supertype
        log("Failed to load %s. [%s]".format(sym.fullName, e.getMessage))
        if (settings.debug.value) { e.printStackTrace }

        false
    }
  }
}
