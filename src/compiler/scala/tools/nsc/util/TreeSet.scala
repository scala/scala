/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package util

/** Sets implemented as binary trees.
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
class TreeSet[T >: Null <: AnyRef](less: (T, T) => Boolean) extends Set[T] {

  private class Tree(val elem: T) {
    var l: Tree = null
    var r: Tree = null
  }

  private var tree: Tree = null

  def findEntry(x: T): T = {
    def find(t: Tree): T = {
      if (t eq null) null
      else if (less(x, t.elem)) find(t.l)
      else if (less(t.elem, x)) find(t.r)
      else t.elem
    }
    find(tree)
  }

  def addEntry(x: T) {
    def add(t: Tree): Tree = {
      if (t eq null) new Tree(x)
      else if (less(x, t.elem)) { t.l = add(t.l); t }
      else if (less(t.elem, x)) { t.r = add(t.r); t }
      else t
    }
    tree = add(tree)
  }

  def iterator = {
    def elems(t: Tree): Iterator[T] = {
      var it = Iterator single t.elem
      if (t.l ne null) it = elems(t.l) append it
      if (t.r ne null) it = it append elems(t.r)
      // if (t.l ne null) it = elems(t.l) ++ it
      // if (t.r ne null) it = it ++ elems(t.r)
      it
    }
    if (tree eq null) Iterator.empty else elems(tree)
  }

  override def toString(): String = {
    if (tree eq null) "<empty>" else "(..." + tree.elem + "...)"
  }
}
