/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.util;

/** Sets implemented as binary trees.
 */
class TreeSet[T <: AnyRef](less: (T, T) => boolean) extends Set[T] {

  private class Tree(val elem: T) {
    var l: Tree = null;
    var r: Tree = null;
  }

  private var tree: Tree = null;

  def findEntry(x: T): T = {
    def find(t: Tree): T = {
      if (t == null) null
      else if (less(x, t.elem)) find(t.l)
      else if (less(t.elem, x)) find(t.r)
      else t.elem
    }
    find(tree)
  }

  def addEntry(x: T): unit = {
    def add(t: Tree): Tree = {
      if (t == null) new Tree(x)
      else if (less(x, t.elem)) { t.l = add(t.l); t }
      else if (less(t.elem, x)) { t.r = add(t.r); t }
      else t
    }
    tree = add(tree)
  }

  def elements = {
    def elems(t: Tree): Iterator[T] = {
      var it = Iterator.single(t.elem);
      if (t.l != null) it = elems(t.l) append it;
      if (t.r != null) it = it append elems(t.r);
      it
    }
    if (tree == null) Iterator.empty else elems(tree)
  }

  override def toString(): String = {
    if (tree == null) "<empty>" else "(..." + tree.elem + "...)";
  }
}
