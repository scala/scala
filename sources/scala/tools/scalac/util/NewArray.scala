/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.ast._;
import scalac.symtab._;
import scalac.util.Name;

package scala.tools.scalac.util {

object NewArray {

  def Tree(xs: Tree*): Array[Tree] = {
    val arr = new Array[Tree](xs.length);
    xs.elements.copyToArray(arr, 0);
    arr
  }

  def TreeArray(xs: Array[Tree]*): Array[Array[Tree]] = {
    val arr = new Array[Array[Tree]](xs.length);
    xs.elements.copyToArray(arr, 0);
    arr
  }

  def ValDef(xs: Tree$ValDef*): Array[Tree$ValDef] = {
    val arr = new Array[Tree$ValDef](xs.length);
    xs.elements.copyToArray(arr, 0);
    arr
  }

  def ValDefArray(xs: Array[Tree$ValDef]*): Array[Array[Tree$ValDef]] = {
    val arr = new Array[Array[Tree$ValDef]](xs.length);
    xs.elements.copyToArray(arr, 0);
    arr
  }

  def CaseDef(xs: Tree$CaseDef*): Array[Tree$CaseDef] = {
    val arr = new Array[Tree$CaseDef](xs.length);
    xs.elements.copyToArray(arr, 0);
    arr
  }

  def Name(xs: Name*): Array[Name] = {
    val arr = new Array[Name](xs.length);
    xs.elements.copyToArray(arr, 0);
    arr
  }

  def Symbol(xs: Symbol*): Array[Symbol] = {
    val arr = new Array[Symbol](xs.length);
    xs.elements.copyToArray(arr, 0);
    arr
  }

  def SymbolArray(xs: Array[Symbol]*): Array[Array[Symbol]] = {
    val arr = new Array[Array[Symbol]](xs.length);
    xs.elements.copyToArray(arr, 0);
    arr
  }

  def Type(xs: Type*): Array[Type] = {
    val arr = new Array[Type](xs.length);
    xs.elements.copyToArray(arr, 0);
    arr
  }

}
}
