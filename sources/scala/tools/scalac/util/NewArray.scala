/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.ast._;
import scalac.checkers.Checker;
import scalac.{symtab => scalac_symtab}
import scalac.util.Name;

package scala.tools.scalac.util {

import scalac_symtab.Symbol;
import scalac_symtab.Type;

object NewArray {

  def Tree(xs: Tree*): Array[Tree] = {
    val arr = new Array[Tree](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }

  def TreeArray(xs: Array[Tree]*): Array[Array[Tree]] = {
    val arr = new Array[Array[Tree]](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }

  def ValDef(xs: Tree$ValDef*): Array[Tree$ValDef] = {
    val arr = new Array[Tree$ValDef](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }

  def ValDefArray(xs: Array[Tree$ValDef]*): Array[Array[Tree$ValDef]] = {
    val arr = new Array[Array[Tree$ValDef]](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }

  def CaseDef(xs: Tree$CaseDef*): Array[Tree$CaseDef] = {
    val arr = new Array[Tree$CaseDef](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }

  def Name(xs: Name*): Array[Name] = {
    val arr = new Array[Name](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }

  def Symbol(xs: Symbol*): Array[Symbol] = {
    val arr = new Array[Symbol](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }

  def SymbolArray(xs: Array[Symbol]*): Array[Array[Symbol]] = {
    val arr = new Array[Array[Symbol]](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }

  def Type(xs: Type*): Array[Type] = {
    val arr = new Array[Type](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }

  def Checker(xs: Checker*): Array[Checker] = {
    val arr = new Array[Checker](xs.length);
    var i = 0;
    for (val t <- xs.elements) {
      arr(i) = t; i = i + 1;
    }
    arr
  }
}
}
