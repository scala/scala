/* NSC -- new Scala compiler
 * Copyright 2007-2008 LAMP/EPFL
 * @author  Sean McDirmid
 */
// $Id$

package scala.tools.nsc.doc

import java.util.zip.ZipFile

import scala.collection.jcl
import symtab.Flags._
import scala.xml._

/**
 *  @author Geoffrey Washburn
 *  This is an abstract class for documentation plugins.
 */
abstract class DocDriver {
 val global: Global
 import global._

 def process(settings: Settings, units: Iterator[CompilationUnit]) : Unit
}
