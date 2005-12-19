/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab.classfile;

import scala.tools.util.{AbstractFile, AbstractFileReader};

import java.io.IOException;

abstract class SymblfileParser {

  val global: Global;
  import global._;

  private var current: AbstractFile = null;       // lock to detect recursive reads

  private object unpickler extends UnPickler {
    val global: SymblfileParser.this.global.type = SymblfileParser.this.global
  }

  def parse(file: AbstractFile, root: Symbol): unit = {
    assert(current == null, current);
    current = file;
    val in = new AbstractFileReader(file);
    if (root.isModule) unpickler.unpickle(in.buf, 0, root.linkedClass, root)
    else unpickler.unpickle(in.buf, 0, root, root.linkedModule);
    current = null
  }
}
