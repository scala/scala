/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc
package doc

/**
 *  This is an abstract class for documentation plugins.
 *
 *  @author Geoffrey Washburn
 */
abstract class DocDriver {
 val global: Global
 import global._
 def settings: doc.Settings

 def process(units: Iterator[CompilationUnit]): Unit
}
