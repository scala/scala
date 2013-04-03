/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
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

  def iterator = toList.iterator

  override def foreach[U](f: T => U) {
    def loop(t: Tree) {
      if (t ne null) {
        loop(t.l)
        f(t.elem)
        loop(t.r)
      }
    }
    loop(tree)
  }
  override def toList = {
    val xs = scala.collection.mutable.ListBuffer[T]()
    foreach(xs += _)
    xs.toList
  }

  override def toString(): String = {
    if (tree eq null) "<empty>" else "(..." + tree.elem + "...)"
  }
}
