/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.symtab.classfile;

/* A wrapper for constant values */
case class Constant(value: Any) {
  // todo: change hashcode generation to make this superfluous
  override def hashCode(): int = if (value == null) 0 else value.hashCode() * 41 + 17;
}

