/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.ast;

import scala.tools.util.Position;

abstract class TreeGen {

  val global: Global;

  import global._;

  def mkGlobalRef(sym: Symbol): Tree = EmptyTree;

  def This(pos: int, sym: Symbol): Tree = EmptyTree;
}
