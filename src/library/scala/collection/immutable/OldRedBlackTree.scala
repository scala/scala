/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.immutable

import java.io.IOException
import scala.annotation.meta.getter

private[immutable] object RedBlackTree {

  // Trees for serialisation compat with 2.12 legacy format
  // allow the format to be written in that manner for 2.12.11 and before
  //on write this is the same format as before
  //on read the `readResolve` will convert to the NewRedBlackTree format

  // the Tree children must be AnyRef as during construction then are RedBlackTree.Tree
  // due to the readResolve the tree is migrated to new format and the children will be converted to
  // NewRedBlackTree as they are read

  @SerialVersionUID(7757490705548110898L)
  sealed abstract class Tree[A, +B](
                                     @(inline@getter) final val key: A,
                                     @(inline@getter) final val value: B,
                                     @(inline@getter) final val left: AnyRef,
                                     @(inline@getter) final val right: AnyRef)
    extends Serializable {
    private def _count(tree: AnyRef) = if (tree eq null) 0 else tree.asInstanceOf[Tree[A, B]].count
    @(inline @getter) final val count: Int = 1 + _count(left) + _count(right)
  }

  @SerialVersionUID(6516527240275040268L)
  final class RedTree[A, +B](key: A,
                             value: B,
                             left: AnyRef,
                             right: AnyRef) extends Tree[A, B](key, value, left, right) {
    @throws[IOException]
    private[this] def readResolve(): AnyRef =
      NewRedBlackTree.RedTree(key, value,
        this.left.asInstanceOf[NewRedBlackTree.Tree[A, B]],
        this.right.asInstanceOf[NewRedBlackTree.Tree[A, B]])

    override def toString: String = "RedTree(" + key + ", " + value + ", " + left + ", " + right + ")"
  }

  @SerialVersionUID(-3666942709716265983L)
  final class BlackTree[A, +B](key: A,
                               value: B,
                               left: AnyRef,
                               right: AnyRef) extends Tree[A, B](key, value, left, right) {
    @throws[IOException]
    private[this] def readResolve(): AnyRef =
      NewRedBlackTree.BlackTree(key, value,
        this.left.asInstanceOf[NewRedBlackTree.Tree[A, B]],
        this.right.asInstanceOf[NewRedBlackTree.Tree[A, B]])

    override def toString: String = "BlackTree(" + key + ", " + value + ", " + left + ", " + right + ")"
  }

  def from[A, B](tree: NewRedBlackTree.Tree[A, B]): Tree[A, B] = {
    if (tree eq null) null
    else {
      val left = from(tree.left)
      val right = from(tree.right)
      if (NewRedBlackTree.isBlack(tree))
        new BlackTree(tree.key, tree.value, left, right)
      else
        new RedTree(tree.key, tree.value, left, right)
    }
  }

}