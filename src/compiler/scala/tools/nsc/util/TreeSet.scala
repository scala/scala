/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author  Martin Odersky
 */

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
      if (t eq null) Iterator.empty
      else elems(t.l) ++ (Iterator single t.elem) ++ elems(t.r)
    }
    elems(tree)
  }

  override def toString(): String = {
    if (tree eq null) "<empty>" else "(..." + tree.elem + "...)"
  }
}
