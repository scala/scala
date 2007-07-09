/* NSC -- new Scala compiler
 * Copyright 2006-2007 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

object TagIndexPair {
  /** inserts tag and index, maintaining relative order of tags */
  def insert(current: TagIndexPair, tag: Int, index: Int): TagIndexPair = {
    if (current eq null)
      new TagIndexPair(tag, index, null)
    else if (tag > current.tag)
      new TagIndexPair(current.tag, current.index, insert(current.next, tag, index))
    else
      new TagIndexPair(tag, index, current)
  }
}

/** sorted, null-terminated list of (int,int) pairs */
class TagIndexPair(val tag: Int, val index: Int, val next: TagIndexPair) {

  def find(tag: Int): Int =
    if (this.tag == tag) index
    else next.find(tag) // assumes argument can always be found

}
