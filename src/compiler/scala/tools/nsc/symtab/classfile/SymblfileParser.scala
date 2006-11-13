/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.symtab.classfile

import scala.tools.nsc.io.AbstractFile

/** This abstract class implements ..
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class SymblfileParser {

  val global: Global
  import global._

  private var current: AbstractFile = null   // lock to detect recursive reads

  private object unpickler extends UnPickler {
    val global: SymblfileParser.this.global.type = SymblfileParser.this.global
  }

  def parse(file: AbstractFile, root: Symbol): unit = {
    assert(current eq null, current)
    current = file
    val bytes  = new AbstractFileReader(file).buf
    if (root.isModule)
      unpickler.unpickle(bytes, 0, root.linkedClassOfModule, root, file.toString())
    else
      unpickler.unpickle(bytes, 0, root, root.linkedModuleOfClass, file.toString())
    current = null
  }
}
