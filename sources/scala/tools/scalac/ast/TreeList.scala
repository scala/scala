/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
\*                                                                      */

// $Id$

import scalac.ast._;

package scala.tools.scalac.ast {

/** List of trees.
 */

final class TreeList(ts: Array[Tree]) with Iterable[Tree] {

  private var trees = ts;
  private var len = ts.length;

  def this() = { this(new Array[Tree](4)); len = 0 }

  def append(tree: Tree): TreeList = {
    if (len == trees.length) {
      val ts = new Array[Tree](if (len == 0) 4 else len * 2);
      System.arraycopy(trees, 0, ts, 0, len);
      trees = ts;
    }
    trees(len) = tree;
    len = len + 1;
    this
  }

  def append(tts: Array[Tree]): unit = {
    for (val j <- Iterator.range(0, tts.length))
      append(tts(j));
  }

  def append(tl: TreeList): unit = {
    for (val j <- Iterator.range(0, tl.len))
      append(tl.trees(j));
  }

  def clear(): unit = {
    trees = new Array[Tree](4);
    len = 0;
  }

  def elements = new Iterator[Tree] { // don't change treelist while iterating!
    private var i = 0;
    def hasNext: Boolean = i < len;
    def next: Tree =
      if (i < len) { val x = trees(i) ; i = i + 1 ; x }
      else error("next on empty iterator");
  }

  def length(): int = len;

  def get(i: int): Tree = trees(i);

  def first(): Tree = trees(0);

  def removeLast(): Tree = {
    len = len - 1;
    trees(len)
  }

  def toArray(): Array[Tree] = {
    val tts = new Array[Tree](len);
    System.arraycopy(trees, 0, tts, 0, len);
    tts
  }

  def copyTo[t <: Tree](ts: Array[t]): Array[t] = copyTo(ts, 0);

  def copyTo[t <: Tree](ts: Array[t], from: int): Array[t] = {
    System.arraycopy(trees, 0, ts, from, len);
    ts;
  }

  override def toString() = {
    Iterator.fromArray(this.toArray()).toList.toString();
  }
}
}
