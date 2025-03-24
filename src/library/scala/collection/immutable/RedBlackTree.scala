/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection
package immutable

import scala.annotation.meta.{getter, setter}
import scala.annotation.tailrec
import scala.runtime.Statics.releaseFence

/** An object containing the RedBlack tree implementation used by for `TreeMaps` and `TreeSets`.
  *
  *  Implementation note: since efficiency is important for data structures this implementation
  *  uses `null` to represent empty trees. This also means pattern matching cannot
  *  easily be used. The API represented by the RedBlackTree object tries to hide these
  *  optimizations behind a reasonably clean API.
  */
private[collection] object RedBlackTree {
  def validate[A](tree: Tree[A, _])(implicit ordering: Ordering[A]): tree.type = {
    def impl(tree: Tree[A, _], keyProp: A => Boolean): Int = {
      assert(keyProp(tree.key), s"key check failed: $tree")
      if (tree.isRed) {
        assert(tree.left == null || tree.left.isBlack, s"red-red left $tree")
        assert(tree.right == null || tree.right.isBlack, s"red-red right $tree")
      }
      val leftBlacks = if (tree.left == null) 0 else impl(tree.left, k => keyProp(k) && ordering.compare(k, tree.key) < 0)
      val rightBlacks = if (tree.right == null) 0 else impl(tree.right, k => keyProp(k) && ordering.compare(k, tree.key) > 0)
      assert(leftBlacks == rightBlacks, s"not balanced: $tree")
      leftBlacks + (if (tree.isBlack) 1 else 0)
    }
    if (tree != null) impl(tree, _ => true)
    tree
  }

  def isEmpty(tree: Tree[_, _]): Boolean = tree eq null

  def contains[A: Ordering](tree: Tree[A, _], x: A): Boolean = lookup(tree, x) ne null
  def get[A: Ordering, B](tree: Tree[A, B], x: A): Option[B] = lookup(tree, x) match {
    case null  => None
    case found => Some(found.value)
  }

  @tailrec
  def lookup[A, B](tree: Tree[A, B], x: A)(implicit ordering: Ordering[A]): Tree[A, B] = if (tree eq null) null else {
    val cmp = ordering.compare(x, tree.key)
    if (cmp < 0) lookup(tree.left, x)
    else if (cmp > 0) lookup(tree.right, x)
    else tree
  }
  private[immutable] abstract class Helper[A](implicit val ordering: Ordering[A]) {
    def beforePublish[B](tree: Tree[A, B]): Tree[A, B] = {
      if (tree eq null) tree
      else if (tree.isMutable) {
        val res = tree.mutableBlack.makeImmutable
        releaseFence()
        res
      } else tree.black
    }
    /** Create a new balanced tree where `newLeft` replaces `tree.left`.
     * tree and newLeft are never null */
    protected[this] final def mutableBalanceLeft[A1, B, B1 >: B](tree: Tree[A1, B], newLeft: Tree[A1, B1]): Tree[A1, B1] = {
      // Parameter trees
      //            tree              |                   newLeft
      //     --      KV         R     |    nl.L            nl.KV      nl.R
      //                              |                     nl.R.L   nl.R.KV  nl.R.R
      //Note - unlike the immutable trees we can't consider tree.left eq newLeft
      //as the balance operations may mutate the same object
      //but that check was mostly to avoid the object creation
      if (newLeft.isRed) {
        val newLeft_left = newLeft.left
        val newLeft_right = newLeft.right
        if (isRedTree(newLeft_left)) {
          //                      RED
          //    black(nl.L)      nl.KV      black
          //                          nl.R    KV   R
          val resultLeft = newLeft_left.mutableBlack
          val resultRight = tree.mutableBlackWithLeft(newLeft_right)

          newLeft.mutableWithLeftRight(resultLeft, resultRight)
        } else if (isRedTree(newLeft_right)) {
          //                              RED
          //           black            nl.R.KV      black
          //    nl.L   nl.KV  nl.R.L           nl.R.R  KV   R

          val newLeft_right_right = newLeft_right.right

          val resultLeft = newLeft.mutableBlackWithRight(newLeft_right.left)
          val resultRight = tree.mutableBlackWithLeft(newLeft_right_right)

          newLeft_right.mutableWithLeftRight(resultLeft, resultRight)
        } else {
          //               tree
          //    newLeft      KV         R
          tree.mutableWithLeft(newLeft)
        }
      } else {
        //                tree
        //     newLeft      KV         R
        tree.mutableWithLeft(newLeft)
      }
    }
    /** Create a new balanced tree where `newRight` replaces `tree.right`.
     * tree and newRight are never null */
    protected[this] final def mutableBalanceRight[A1, B, B1 >: B](tree: Tree[A1, B], newRight: Tree[A1, B1]): Tree[A1, B1] = {
      // Parameter trees
      //            tree                |                             newRight
      //      L      KV         --      |              nr.L            nr.KV      nr.R
      //                                |     nr.L.L   nr.L.KV  nr.L.R
      //Note - unlike the immutable trees we can't consider tree.right eq newRight
      //as the balance operations may mutate the same object
      //but that check was mostly to avoid the object creation
      if (newRight.isRed) {
        val newRight_left = newRight.left
        if (isRedTree(newRight_left)) {
          //                                RED
          //              black           nr.L.KV          black
          //     L         KV   nr.L.L             nr.L.R  nr.KV    nr.R

          val resultLeft = tree.mutableBlackWithRight(newRight_left.left)
          val resultRight = newRight.mutableBlackWithLeft(newRight_left.right)

          newRight_left.mutableWithLeftRight(resultLeft, resultRight)

        } else {
          val newRight_right = newRight.right
          if (isRedTree(newRight_right)) {
            //                                RED
            //              black           nr.KV            black(nr.R)
            //     L         KV   nr.L

            val resultLeft = tree.mutableBlackWithRight(newRight_left)
            val resultRight = newRight_right.mutableBlack

            newRight.mutableWithLeftRight(resultLeft, resultRight)
          } else {
            //             tree
            //     L        KV         newRight
            tree.mutableWithRight(newRight)
          }
        }
      } else {
        //               tree
        //       L        KV         newRight
        tree.mutableWithRight(newRight)
      }
    }
  }
  private[immutable] class SetHelper[A](implicit ordering: Ordering[A]) extends Helper[A] {
    protected[this] final def mutableUpd(tree: Tree[A, Any], k: A): Tree[A, Any] =
      if (tree eq null) {
        mutableRedTree(k, (), null, null)
      } else if (k.asInstanceOf[AnyRef] eq tree.key.asInstanceOf[AnyRef]) {
        tree
      } else {
        val cmp = ordering.compare(k, tree.key)
        if (cmp < 0)
          mutableBalanceLeft(tree, mutableUpd(tree.left, k))
        else if (cmp > 0)
          mutableBalanceRight(tree, mutableUpd(tree.right, k))
        else tree
      }
  }
  private[immutable] class MapHelper[A, B](implicit ordering: Ordering[A]) extends Helper[A] {
    protected[this] final def mutableUpd[B1 >: B](tree: Tree[A, B], k: A, v: B1): Tree[A, B1] =
      if (tree eq null) {
        mutableRedTree(k, v, null, null)
      } else if (k.asInstanceOf[AnyRef] eq tree.key.asInstanceOf[AnyRef]) {
        tree.mutableWithV(v)
      } else {
        val cmp = ordering.compare(k, tree.key)
        if (cmp < 0)
          mutableBalanceLeft(tree, mutableUpd(tree.left, k, v))
        else if (cmp > 0)
          mutableBalanceRight(tree, mutableUpd(tree.right, k, v))
        else tree.mutableWithV(v)
      }
  }

  def count(tree: Tree[_, _]) = if (tree eq null) 0 else tree.count
  def update[A: Ordering, B, B1 >: B](tree: Tree[A, B], k: A, v: B1, overwrite: Boolean): Tree[A, B1] = blacken(upd(tree, k, v, overwrite))
  def delete[A: Ordering, B](tree: Tree[A, B], k: A): Tree[A, B] = blacken(del(tree, k))
  def rangeImpl[A: Ordering, B](tree: Tree[A, B], from: Option[A], until: Option[A]): Tree[A, B] = (from, until) match {
    case (Some(from), Some(until)) => this.range(tree, from, until)
    case (Some(from), None)        => this.from(tree, from)
    case (None,       Some(until)) => this.until(tree, until)
    case (None,       None)        => tree
  }
  def range[A: Ordering, B](tree: Tree[A, B], from: A, until: A): Tree[A, B] = blacken(doRange(tree, from, until))
  def from[A: Ordering, B](tree: Tree[A, B], from: A): Tree[A, B] = blacken(doFrom(tree, from))
  def to[A: Ordering, B](tree: Tree[A, B], to: A): Tree[A, B] = blacken(doTo(tree, to))
  def until[A: Ordering, B](tree: Tree[A, B], key: A): Tree[A, B] = blacken(doUntil(tree, key))

  def drop[A: Ordering, B](tree: Tree[A, B], n: Int): Tree[A, B] = blacken(doDrop(tree, n))
  def take[A: Ordering, B](tree: Tree[A, B], n: Int): Tree[A, B] = blacken(doTake(tree, n))
  def slice[A: Ordering, B](tree: Tree[A, B], from: Int, until: Int): Tree[A, B] = blacken(doSlice(tree, from, until))

  def smallest[A, B](tree: Tree[A, B]): Tree[A, B] = {
    if (tree eq null) throw new NoSuchElementException("empty tree")
    var result = tree
    while (result.left ne null) result = result.left
    result
  }
  def greatest[A, B](tree: Tree[A, B]): Tree[A, B] = {
    if (tree eq null) throw new NoSuchElementException("empty tree")
    var result = tree
    while (result.right ne null) result = result.right
    result
  }

  def tail[A, B](tree: Tree[A, B]): Tree[A, B] = {
    def _tail(tree: Tree[A, B]): Tree[A, B] =
      if (tree eq null) throw new NoSuchElementException("empty tree")
      else {
        val tl = tree.left
        if (tl eq null) tree.right
        else if (tl.isBlack) balLeft(tree, _tail(tl), tree.right)
        else tree.redWithLeft(_tail(tree.left))
      }
    blacken(_tail(tree))
  }

  def init[A, B](tree: Tree[A, B]): Tree[A, B] = {
    def _init(tree: Tree[A, B]): Tree[A, B] =
      if (tree eq null) throw new NoSuchElementException("empty tree")
      else {
        val tr = tree.right
        if (tr eq null) tree.left
        else if (tr.isBlack) balRight(tree, tree.left, _init(tr))
        else tree.redWithRight(_init(tr))
      }
    blacken(_init(tree))
  }

  /**
    * Returns the smallest node with a key larger than or equal to `x`. Returns `null` if there is no such node.
    */
  def minAfter[A, B](tree: Tree[A, B], x: A)(implicit ordering: Ordering[A]): Tree[A, B] = if (tree eq null) null else {
    val cmp = ordering.compare(x, tree.key)
    if (cmp == 0) tree
    else if (cmp < 0) {
      val l = minAfter(tree.left, x)
      if (l != null) l else tree
    } else minAfter(tree.right, x)
  }

  /**
    * Returns the largest node with a key smaller than `x`. Returns `null` if there is no such node.
    */
  def maxBefore[A, B](tree: Tree[A, B], x: A)(implicit ordering: Ordering[A]): Tree[A, B] = if (tree eq null) null else {
    val cmp = ordering.compare(x, tree.key)
    if (cmp <= 0) maxBefore(tree.left, x)
    else {
      val r = maxBefore(tree.right, x)
      if (r != null) r else tree
    }
  }

  def foreach[A,B,U](tree:Tree[A,B], f:((A,B)) => U):Unit = if (tree ne null) _foreach(tree,f)

  def keysEqual[A: Ordering, X, Y](a: Tree[A, X], b: Tree[A, Y]): Boolean = {
    if (a eq b) true
    else if (a eq null) false
    else if (b eq null) false
    else a.count == b.count && (new EqualsIterator(a)).sameKeys(new EqualsIterator(b))
  }
  def valuesEqual[A: Ordering, X, Y](a: Tree[A, X], b: Tree[A, Y]): Boolean = {
    if (a eq b) true
    else if (a eq null) false
    else if (b eq null) false
    else a.count == b.count && (new EqualsIterator(a)).sameValues(new EqualsIterator(b))
  }
  def entriesEqual[A: Ordering, X, Y](a: Tree[A, X], b: Tree[A, Y]): Boolean = {
    if (a eq b) true
    else if (a eq null) false
    else if (b eq null) false
    else a.count == b.count && (new EqualsIterator(a)).sameEntries(new EqualsIterator(b))
  }

  private[this] def _foreach[A, B, U](tree: Tree[A, B], f: ((A, B)) => U): Unit = {
    if (tree.left ne null) _foreach(tree.left, f)
    f((tree.key, tree.value))
    if (tree.right ne null) _foreach(tree.right, f)
  }

  def foreachKey[A, U](tree:Tree[A,_], f: A => U):Unit = if (tree ne null) _foreachKey(tree,f)

  private[this] def _foreachKey[A, U](tree: Tree[A, _], f: A => U): Unit = {
    if (tree.left ne null) _foreachKey(tree.left, f)
    f((tree.key))
    if (tree.right ne null) _foreachKey(tree.right, f)
  }

  def foreachEntry[A, B, U](tree:Tree[A,B], f: (A, B) => U):Unit = if (tree ne null) _foreachEntry(tree,f)

  private[this] def _foreachEntry[A, B, U](tree: Tree[A, B], f: (A, B) => U): Unit = {
    if (tree.left ne null) _foreachEntry(tree.left, f)
    f(tree.key, tree.value)
    if (tree.right ne null) _foreachEntry(tree.right, f)
  }

  def iterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None): Iterator[(A, B)] = new EntriesIterator(tree, start)
  def keysIterator[A: Ordering](tree: Tree[A, _], start: Option[A] = None): Iterator[A] = new KeysIterator(tree, start)
  def valuesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None): Iterator[B] = new ValuesIterator(tree, start)

  @tailrec
  def nth[A, B](tree: Tree[A, B], n: Int): Tree[A, B] = {
    val count = this.count(tree.left)
    if (n < count) nth(tree.left, n)
    else if (n > count) nth(tree.right, n - count - 1)
    else tree
  }

  def isBlack(tree: Tree[_, _]) = (tree eq null) || tree.isBlack

  @`inline` private[this] def isRedTree(tree: Tree[_, _]) = (tree ne null) && tree.isRed
  @`inline` private[this] def isBlackTree(tree: Tree[_, _]) = (tree ne null) && tree.isBlack

  private[this] def blacken[A, B](t: Tree[A, B]): Tree[A, B] = if (t eq null) null else t.black

  // Blacken if the tree is red and has a red child. This is necessary when using methods such as `upd` or `updNth`
  // for building subtrees. Use `blacken` instead when building top-level trees.
  private[this] def maybeBlacken[A, B](t: Tree[A, B]): Tree[A, B] =
    if(isBlack(t)) t else if(isRedTree(t.left) || isRedTree(t.right)) t.black else t

  private[this] def mkTree[A, B](isBlack: Boolean, key: A, value: B, left: Tree[A, B], right: Tree[A, B]) = {
    val sizeAndColour = sizeOf(left) + sizeOf(right) + 1 | (if(isBlack) initialBlackCount else initialRedCount)
    new Tree(key, value.asInstanceOf[AnyRef], left, right, sizeAndColour)
  }

  /** Create a new balanced tree where `newLeft` replaces `tree.left`. */
  private[this] def balanceLeft[A, B1](tree: Tree[A, B1], newLeft: Tree[A, B1]): Tree[A, B1] = {
    // Parameter trees
    //            tree              |                   newLeft
    //     --      KV         R     |    nl.L            nl.KV      nl.R
    //                              |                     nl.R.L   nl.R.KV  nl.R.R
    if (tree.left eq newLeft) tree
    else {
      if (newLeft.isRed) {
        val newLeft_left = newLeft.left
        val newLeft_right = newLeft.right
        if (isRedTree(newLeft_left)) {
          //                      RED
          //    black(nl.L)      nl.KV      black
          //                          nl.R    KV   R
          val resultLeft = newLeft_left.black
          val resultRight = tree.blackWithLeft(newLeft_right)

          newLeft.withLeftRight(resultLeft, resultRight)
        } else if (isRedTree(newLeft_right)) {
          //                              RED
          //           black            nl.R.KV      black
          //    nl.L   nl.KV  nl.R.L           nl.R.R  KV   R
          val newLeft_right_right = newLeft_right.right

          val resultLeft = newLeft.blackWithRight(newLeft_right.left)
          val resultRight = tree.blackWithLeft(newLeft_right_right)

          newLeft_right.withLeftRight(resultLeft, resultRight)
        } else {
          //               tree
          //    newLeft      KV         R
          tree.withLeft(newLeft)
        }
      } else {
        //                tree
        //     newLeft      KV         R
        tree.withLeft(newLeft)
      }
    }
  }
  /** Create a new balanced tree where `newRight` replaces `tree.right`. */
  private[this] def balanceRight[A, B1](tree: Tree[A, B1], newRight: Tree[A, B1]): Tree[A, B1] = {
    // Parameter trees
    //            tree                |                             newRight
    //      L      KV         --      |              nr.L            nr.KV      nr.R
    //                                |     nr.L.L   nr.L.KV  nr.L.R
    if (tree.right eq newRight) tree
    else {
      if (newRight.isRed) {
        val newRight_left = newRight.left
        if (isRedTree(newRight_left)) {
          //                                RED
          //              black           nr.L.KV          black
          //     L         KV   nr.L.L             nr.L.R  nr.KV    nr.R
          val resultLeft = tree.blackWithRight(newRight_left.left)
          val resultRight = newRight.blackWithLeft(newRight_left.right)

          newRight_left.withLeftRight(resultLeft, resultRight)
        } else {
          val newRight_right = newRight.right
          if (isRedTree(newRight_right)) {
            //                                RED
            //              black           nr.KV            black(nr.R)
            //     L         KV   nr.L
            val resultLeft = tree.blackWithRight(newRight_left)
            val resultRight = newRight_right.black

            newRight.withLeftRight(resultLeft, resultRight)
          } else {
            //             tree
            //     L        KV         newRight
            tree.withRight(newRight)
          }
        }
      } else {
        //               tree
        //       L        KV         newRight
        tree.withRight(newRight)
      }
    }
  }

  private[this] def upd[A, B, B1 >: B](tree: Tree[A, B], k: A, v: B1, overwrite: Boolean)(implicit ordering: Ordering[A]): Tree[A, B1] = if (tree eq null) {
    RedTree(k, v, null, null)
  } else if (k.asInstanceOf[AnyRef] eq tree.key.asInstanceOf[AnyRef]) {
    if (overwrite)
      tree.withV(v)
    else tree
  } else {
    val cmp = ordering.compare(k, tree.key)
    if (cmp < 0)
      balanceLeft(tree, upd(tree.left, k, v, overwrite))
    else if (cmp > 0)
      balanceRight(tree, upd(tree.right, k, v, overwrite))
    else if (overwrite && (v.asInstanceOf[AnyRef] ne tree.value.asInstanceOf[AnyRef]))
                        tree.withV(v)
    else tree
  }
  private[this] def updNth[A, B, B1 >: B](tree: Tree[A, B], idx: Int, k: A, v: B1): Tree[A, B1] = if (tree eq null) {
    RedTree(k, v, null, null)
  } else {
    val rank = count(tree.left) + 1
    if (idx < rank)
      balanceLeft(tree, updNth(tree.left, idx, k, v))
    else if (idx > rank)
      balanceRight(tree, updNth(tree.right, idx - rank, k, v))
    else tree
  }

  private[this] def doFrom[A, B](tree: Tree[A, B], from: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
    if (tree eq null) return null
    if (ordering.lt(tree.key, from)) return doFrom(tree.right, from)
    val newLeft = doFrom(tree.left, from)
    if (newLeft eq tree.left) tree
    else if (newLeft eq null) maybeBlacken(upd(tree.right, tree.key, tree.value, overwrite = false))
    else join(newLeft, tree.key, tree.value, tree.right)
  }
  private[this] def doTo[A, B](tree: Tree[A, B], to: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
    if (tree eq null) return null
    if (ordering.lt(to, tree.key)) return doTo(tree.left, to)
    val newRight = doTo(tree.right, to)
    if (newRight eq tree.right) tree
    else if (newRight eq null) maybeBlacken(upd(tree.left, tree.key, tree.value, overwrite = false))
    else join(tree.left, tree.key, tree.value, newRight)
  }
  private[this] def doUntil[A, B](tree: Tree[A, B], until: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
    if (tree eq null) return null
    if (ordering.lteq(until, tree.key)) return doUntil(tree.left, until)
    val newRight = doUntil(tree.right, until)
    if (newRight eq tree.right) tree
    else if (newRight eq null) maybeBlacken(upd(tree.left, tree.key, tree.value, overwrite = false))
    else join(tree.left, tree.key, tree.value, newRight)
  }

  private[this] def doRange[A, B](tree: Tree[A, B], from: A, until: A)(implicit ordering: Ordering[A]): Tree[A, B] = {
    if (tree eq null) return null
    if (ordering.lt(tree.key, from)) return doRange(tree.right, from, until)
    if (ordering.lteq(until, tree.key)) return doRange(tree.left, from, until)
    val newLeft = doFrom(tree.left, from)
    val newRight = doUntil(tree.right, until)
    if ((newLeft eq tree.left) && (newRight eq tree.right)) tree
    else if (newLeft eq null) upd(newRight, tree.key, tree.value, overwrite = false)
    else if (newRight eq null) upd(newLeft, tree.key, tree.value, overwrite = false)
    else join(newLeft, tree.key, tree.value, newRight)
  }

  private[this] def doDrop[A, B](tree: Tree[A, B], n: Int): Tree[A, B] =
    if((tree eq null) || (n <= 0)) tree
    else if(n >= tree.count) null
    else {
      val l = count(tree.left)
      if(n > l) doDrop(tree.right, n-l-1)
      else if(n == l) join(null, tree.key, tree.value, tree.right)
      else join(doDrop(tree.left, n), tree.key, tree.value, tree.right)
    }

  private[this] def doTake[A, B](tree: Tree[A, B], n: Int): Tree[A, B] =
    if((tree eq null) || (n <= 0)) null
    else if(n >= tree.count) tree
    else {
      val l = count(tree.left)
      if(n <= l) doTake(tree.left, n)
      else if(n == l+1) maybeBlacken(updNth(tree.left, n, tree.key, tree.value))
      else join(tree.left, tree.key, tree.value, doTake(tree.right, n-l-1))
    }

  private[this] def doSlice[A, B](tree: Tree[A, B], from: Int, until: Int): Tree[A, B] =
    if((tree eq null) || (from >= until) || (from >= tree.count) || (until <= 0)) null
    else if((from <= 0) && (until >= tree.count)) tree
    else {
      val l = count(tree.left)
      if(until <= l) doSlice(tree.left, from, until)
      else if(from > l) doSlice(tree.right, from-l-1, until-l-1)
      else join(doDrop(tree.left, from), tree.key, tree.value, doTake(tree.right, until-l-1))
    }

  /*
   * Forcing direct fields access using the @`inline` annotation helps speed up
   * various operations (especially smallest/greatest and update/delete).
   *
   * Unfortunately the direct field access is not guaranteed to work (but
   * works on the current implementation of the Scala compiler).
   *
   * An alternative is to implement the these classes using plain old Java code...
   *
   * Mutability
   * This implementation encodes both mutable and immutable trees.
   * Mutable trees are never exposed to the user code but we get significant reductions in both CPU and allocations
   * by maintaining a mutable tree during internal operations, e.g. a builder building a Tree, and the other bulk
   * API such as filter or ++
   *
   * Mutable trees are only used within the confines of this bulk operation and not shared
   * Mutable trees may transition to become immutable by calling beforePublish
   * Mutable trees may have child nodes (left and right) which are immutable Trees (this promotes structural sharing)
   *
   * Immutable trees may only child nodes (left and right) which are immutable Trees, and as such the immutable
   * trees the entire transitive subtree is immutable
   *
   * Colour, mutablity and size encoding
   * The colour of the Tree, its mutablity and size are all encoded in the _count field
   * The colour is encoded in the top bit (31) of the _count. This allows a mutable tree to change colour without
   *   additional allocation
   * The mutable trees always have bits 0 .. 30 (inclusive) set to 0
   * The immutable trees always have bits 0 .. 30 containing the size of the transitive subtree
   *
   * Naming
   * All of the methods that can yield a mutable result have "mutable" on their name, and generally there
   * is another method similarly named with doesn't. This is to aid safety and to reduce the cognitive load when
   * reviewing changes. e.g.
   *  def        upd(...) will update an immutable Tree, producing an immutable Tree
   *  def mutableUpd(...) will update a mutable or immutable Tree and may return a mutable or immutable Tree
   * a method that has mutable in its name may return a immutable tree if the operation can reuse the existing tree
   *
   */
  private[immutable] final class Tree[A, +B](
                           @(`inline` @getter @setter)     private var _key: A,
                           @(`inline` @getter @setter)     private var _value: AnyRef,
                           @(`inline` @getter @setter)     private var _left: Tree[A, _],
                           @(`inline` @getter @setter)     private var _right: Tree[A, _],
                           @(`inline` @getter @setter)     private var _count: Int)
  {
    @`inline` private[RedBlackTree] def isMutable: Boolean = (_count & colourMask) == 0
    // read only APIs
    @`inline` private[RedBlackTree] final def count = {
      //devTimeAssert((_count & 0x7FFFFFFF) != 0)
      _count & colourMask
    }
    //retain the colour, and mark as mutable
    @`inline` private def mutableRetainingColour = _count & colourBit

    //inlined here to avoid outer object null checks
    @`inline` private[RedBlackTree] final def sizeOf(tree:Tree[_,_]) = if (tree eq null) 0 else tree.count
    @`inline` private[immutable] final def key = _key
    @`inline` private[immutable] final def value = _value.asInstanceOf[B]
    @`inline` private[immutable] final def left = _left.asInstanceOf[Tree[A, B]]
    @`inline` private[immutable] final def right = _right.asInstanceOf[Tree[A, B]]
    //Note - only used in tests outside RedBlackTree
    @`inline` private[immutable] final def isBlack = _count < 0
    //Note - only used in tests outside RedBlackTree
    @`inline` private[immutable] final def isRed = _count >= 0

    override def toString: String = s"${if(isRed) "RedTree" else "BlackTree"}($key, $value, $left, $right)"

    //mutable APIs
    private[RedBlackTree] def makeImmutable: this.type = {
      def makeImmutableImpl(): Unit = {
        if (isMutable) {
          var size = 1
          if (_left ne null) {
            _left.makeImmutable
            size += _left.count
          }
          if (_right ne null) {
            _right.makeImmutable
            size += _right.count
          }
          _count |= size //retains colour
        }
      }
      makeImmutableImpl()
      this
    }

    private[RedBlackTree] def mutableBlack: Tree[A, B] = {
      if (isBlack) this
      else if (isMutable) {
        _count = initialBlackCount
        this
      }
      else new Tree(_key, _value, _left, _right, initialBlackCount)
    }
//    private[RedBlackTree] def mutableRed: Tree[A, B] = {
//      if (isRed) this
//      else if (mutable) {
//        _count = initialRedCount
//        this
//      }
//      else new Tree(_key, _value, _left, _right, initialRedCount)
//    }

    private[RedBlackTree] def mutableWithV[B1 >: B](newValue: B1): Tree[A, B1] = {
      if (newValue.asInstanceOf[AnyRef] eq _value.asInstanceOf[AnyRef]) this
      else if (isMutable) {
        _value = newValue.asInstanceOf[AnyRef]
        this
      } else new Tree(_key, newValue.asInstanceOf[AnyRef], _left, _right, mutableRetainingColour)
    }

    private[RedBlackTree] def mutableWithLeft[B1 >: B](newLeft: Tree[A, B1]): Tree[A, B1] = {
      if (_left eq newLeft) this
      else if (isMutable) {
        _left = newLeft
        this
      } else new Tree(_key, _value, newLeft, _right, mutableRetainingColour)
    }
    private[RedBlackTree] def mutableWithRight[B1 >: B](newRight: Tree[A, B1]): Tree[A, B1] = {
      if (_right eq newRight) this
      else if (isMutable) {
        _right = newRight
        this
      } else new Tree(_key, _value, _left, newRight, mutableRetainingColour)
    }
    private[RedBlackTree] def mutableWithLeftRight[B1 >: B](newLeft: Tree[A, B1], newRight: Tree[A, B1]): Tree[A, B1] = {
      if ((_left eq newLeft) && (_right eq newRight)) this
      else if (isMutable) {
        _left = newLeft
        _right = newRight
        this
      } else new Tree(_key, _value, newLeft, newRight, mutableRetainingColour)
    }
    private[RedBlackTree] def mutableBlackWithLeft[B1 >: B](newLeft: Tree[A, B1]): Tree[A, B1] = {
      if ((_left eq newLeft) && isBlack) this
      else if (isMutable) {
        _count = initialBlackCount
        _left = newLeft
        this
      } else new Tree(_key, _value, newLeft, _right, initialBlackCount)
    }
    private[RedBlackTree] def mutableBlackWithRight[B1 >: B](newRight: Tree[A, B1]): Tree[A, B1] = {
      if ((_right eq newRight) && isBlack) this
      else if (isMutable) {
        _count = initialBlackCount
        _right = newRight
        this
      } else new Tree(_key, _value, _left, newRight, initialBlackCount)
    }

    private[RedBlackTree] def black: Tree[A, B] = {
      //assertNotMutable(this)
      if (isBlack) this
      else new Tree(_key, _value, _left, _right, _count ^ colourBit)
    }
    private[RedBlackTree] def red: Tree[A, B] = {
      //assertNotMutable(this)
      if (isRed) this
      else new Tree(_key, _value, _left, _right, _count ^ colourBit)
    }
    private[RedBlackTree] def withKV[B1 >: B](newKey: A, newValue: B1): Tree[A, B1] = {
      //assertNotMutable(this)
      if ((newKey.asInstanceOf[AnyRef] eq _key.asInstanceOf[AnyRef]) &&
         (newValue.asInstanceOf[AnyRef] eq _value.asInstanceOf[AnyRef])) this
      else new Tree(newKey, newValue.asInstanceOf[AnyRef], _left, _right, _count)
    }
    private[RedBlackTree] def withV[B1 >: B](newValue: B1): Tree[A, B1] = {
      //assertNotMutable(this)
      if (newValue.asInstanceOf[AnyRef] eq _value.asInstanceOf[AnyRef]) this
      else new Tree(_key, newValue.asInstanceOf[AnyRef], _left, _right, _count)
    }

    private[RedBlackTree] def withLeft[B1 >: B](newLeft: Tree[A, B1]): Tree[A, B1] = {
      //assertNotMutable(this)
      //assertNotMutable(newLeft)
      if (newLeft eq _left) this
      else {
        val size = sizeOf(newLeft) + sizeOf(_right) + 1
        new Tree(key, value.asInstanceOf[AnyRef], newLeft, _right, (_count & colourBit) | size)
      }
    }
    private[RedBlackTree] def withRight[B1 >: B](newRight: Tree[A, B1]): Tree[A, B1] = {
      //assertNotMutable(this)
      //assertNotMutable(newRight)
      if (newRight eq _right) this
      else {
        val size = sizeOf(_left) + sizeOf(newRight) + 1
        new Tree(key, value.asInstanceOf[AnyRef], _left, newRight, (_count & colourBit) | size)
      }
    }
    private[RedBlackTree] def blackWithLeft[B1 >: B](newLeft: Tree[A, B1]): Tree[A, B1] = {
      //assertNotMutable(this)
      //assertNotMutable(newLeft)
      if ((newLeft eq _left) && isBlack) this
      else {
        val size = sizeOf(newLeft) + sizeOf(_right) + 1
        new Tree(key, value.asInstanceOf[AnyRef], newLeft, _right, initialBlackCount | size)
      }
    }
    private[RedBlackTree] def redWithLeft[B1 >: B](newLeft: Tree[A, B1]): Tree[A, B1] = {
      //assertNotMutable(this)
      //assertNotMutable(newLeft)
      if ((newLeft eq _left) && isRed) this
      else {
        val size = sizeOf(newLeft) + sizeOf(_right) + 1
        new Tree(key, value.asInstanceOf[AnyRef], newLeft, _right, initialRedCount | size)
      }
    }
    private[RedBlackTree] def blackWithRight[B1 >: B](newRight: Tree[A, B1]): Tree[A, B1] = {
      //assertNotMutable(this)
      //assertNotMutable(newRight)
      if ((newRight eq _right) && isBlack) this
      else {
        val size = sizeOf(_left) + sizeOf(newRight) + 1
        new Tree(key, value.asInstanceOf[AnyRef], _left, newRight, initialBlackCount | size)
      }
    }
    private[RedBlackTree] def redWithRight[B1 >: B](newRight: Tree[A, B1]): Tree[A, B1] = {
      //assertNotMutable(this)
      //assertNotMutable(newLeft)
      if ((newRight eq _right) && isRed) this
      else {
        val size = sizeOf(_left) + sizeOf(newRight) + 1
        new Tree(key, value.asInstanceOf[AnyRef], _left, newRight, initialRedCount | size)
      }
    }
    private[RedBlackTree] def withLeftRight[B1 >: B](newLeft: Tree[A, B1], newRight: Tree[A, B1]): Tree[A, B1] = {
      //assertNotMutable(this)
      //assertNotMutable(newLeft)
      //assertNotMutable(newRight)
      if ((newLeft eq _left) && (newRight eq _right)) this
      else {
        val size = sizeOf(newLeft) + sizeOf(newRight) + 1
        new Tree(key, value.asInstanceOf[AnyRef], newLeft, newRight, (_count & colourBit) | size)
      }
    }
    private[RedBlackTree] def redWithLeftRight[B1 >: B](newLeft: Tree[A, B1], newRight: Tree[A, B1]): Tree[A, B1] = {
      //assertNotMutable(this)
      //assertNotMutable(newLeft)
      //assertNotMutable(newRight)
      if ((newLeft eq _left) && (newRight eq _right) && isRed) this
      else {
        val size = sizeOf(newLeft) + sizeOf(newRight) + 1
        new Tree(key, value.asInstanceOf[AnyRef], newLeft, newRight, initialRedCount | size)
      }
    }
    private[RedBlackTree] def blackWithLeftRight[B1 >: B](newLeft: Tree[A, B1], newRight: Tree[A, B1]): Tree[A, B1] = {
      //assertNotMutable(this)
      //assertNotMutable(newLeft)
      //assertNotMutable(newRight)
      if ((newLeft eq _left) && (newRight eq _right) && isBlack) this
      else {
        val size = sizeOf(newLeft) + sizeOf(newRight) + 1
        new Tree(key, value.asInstanceOf[AnyRef], newLeft, newRight, initialBlackCount | size)
      }
    }
  }
  //see #Tree docs "Colour, mutablity and size encoding"
  //we make these final vals because the optimiser inlines them, without reference to the enclosing module
  private[RedBlackTree] final val colourBit         = 0x80000000
  private[RedBlackTree] final val colourMask        = ~colourBit
  private[RedBlackTree] final val initialBlackCount = colourBit
  private[RedBlackTree] final val initialRedCount   = 0

  @`inline` private[RedBlackTree] def mutableRedTree[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]) = new Tree[A,B](key, value.asInstanceOf[AnyRef], left, right, initialRedCount)
  @`inline` private[RedBlackTree] def mutableBlackTree[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]) = new Tree[A,B](key, value.asInstanceOf[AnyRef], left, right, initialBlackCount)

  /** create a new immutable red tree.
   * left and right may be null
   */
  private[immutable] def RedTree[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]): Tree[A, B] = {
    //assertNotMutable(left)
    //assertNotMutable(right)
    val size = sizeOf(left) + sizeOf(right) + 1
    new Tree(key, value.asInstanceOf[AnyRef], left, right, initialRedCount | size)
  }
  private[immutable] def BlackTree[A, B](key: A, value: B, left: Tree[A, B], right: Tree[A, B]): Tree[A, B] = {
    //assertNotMutable(left)
    //assertNotMutable(right)
    val size = sizeOf(left) + sizeOf(right) + 1
    new Tree(key, value.asInstanceOf[AnyRef], left, right, initialBlackCount | size)
  }
  @`inline` private def sizeOf(tree:Tree[_,_]) = if (tree eq null) 0 else tree.count
  //immutable APIs
  //assertions - uncomment decls and callers  when changing functionality
  //  private def devTimeAssert(assertion: Boolean) = {
  //    //uncomment this during development of the functionality
  //    assert(assertion)
  //  }
  //  private def assertNotMutable(t:Tree[_,_]) = {
  //    devTimeAssert ((t eq null) || t.count > 0)
  //  }
  private[this] abstract class TreeIterator[A, B, R](root: Tree[A, B], start: Option[A])(protected implicit val ordering: Ordering[A]) extends AbstractIterator[R] {
    protected[this] def nextResult(tree: Tree[A, B]): R

    override def hasNext: Boolean = lookahead ne null

    @throws[NoSuchElementException]
    override def next(): R = {
      val tree = lookahead
      if(tree ne null) {
        lookahead = findLeftMostOrPopOnEmpty(goRight(tree))
        nextResult(tree)
      } else Iterator.empty.next()
    }

    @tailrec
    protected final def findLeftMostOrPopOnEmpty(tree: Tree[A, B]): Tree[A, B] =
      if (tree eq null) popNext()
      else if (tree.left eq null) tree
      else findLeftMostOrPopOnEmpty(goLeft(tree))

    @`inline` private[this] def pushNext(tree: Tree[A, B]): Unit = {
      stackOfNexts(index) = tree
      index += 1
    }
    @`inline` protected final def popNext(): Tree[A, B] = if (index == 0) null else {
      index -= 1
      stackOfNexts(index)
    }

    protected[this] val stackOfNexts = if (root eq null) null else {
      /*
       * According to "Ralf Hinze. Constructing red-black trees" [https://www.cs.ox.ac.uk/ralf.hinze/publications/#P5]
       * the maximum height of a red-black tree is 2*log_2(n + 2) - 2.
       *
       * According to {@see Integer#numberOfLeadingZeros} ceil(log_2(n)) = (32 - Integer.numberOfLeadingZeros(n - 1))
       *
       * Although we don't store the deepest nodes in the path during iteration,
       * we potentially do so in `startFrom`.
       */
      val maximumHeight = 2 * (32 - Integer.numberOfLeadingZeros(root.count + 2 - 1)) - 2
      new Array[Tree[A, B]](maximumHeight)
    }
    private[this] var index = 0
    protected var lookahead: Tree[A, B] = if (start.isDefined) startFrom(start.get) else findLeftMostOrPopOnEmpty(root)

    /**
      * Find the leftmost subtree whose key is equal to the given key, or if no such thing,
      * the leftmost subtree with the key that would be "next" after it according
      * to the ordering. Along the way build up the iterator's path stack so that "next"
      * functionality works.
      */
    private[this] def startFrom(key: A) : Tree[A,B] = if (root eq null) null else {
      @tailrec def find(tree: Tree[A, B]): Tree[A, B] =
        if (tree eq null) popNext()
        else find(
          if (ordering.lteq(key, tree.key)) goLeft(tree)
          else goRight(tree)
        )
      find(root)
    }

    @`inline` private[this] def goLeft(tree: Tree[A, B]) = {
      pushNext(tree)
      tree.left
    }

    @`inline` protected final def goRight(tree: Tree[A, B]) = tree.right
  }

  private[this] class EqualsIterator[A: Ordering, B](tree: Tree[A, B]) extends TreeIterator[A, B, Unit](tree, None) {
    override def nextResult(tree: Tree[A, B]): Nothing = ???

    def sameKeys[X](that:EqualsIterator[A,X]): Boolean = {
      var equal = true
      while (equal && (this.lookahead ne null) && (that.lookahead ne null)) {
        if (this.lookahead eq that.lookahead) {
          this.lookahead = this.popNext()
          that.lookahead = that.popNext()
        } else {
          equal = (this.lookahead.key.asInstanceOf[AnyRef] eq that.lookahead.key.asInstanceOf[AnyRef]) ||
            ordering.equiv(this.lookahead.key, that.lookahead.key)
          this.lookahead =  this.findLeftMostOrPopOnEmpty(this.goRight(this.lookahead))
          that.lookahead =  that.findLeftMostOrPopOnEmpty(that.goRight(that.lookahead))
        }
      }
      equal && (this.lookahead eq null) && (that.lookahead eq null)
    }
    def sameValues[X](that:EqualsIterator[A,X]): Boolean = {
      var equal = true
      while (equal && (this.lookahead ne null) && (that.lookahead ne null)) {
        if (this.lookahead eq that.lookahead) {
          this.lookahead = this.popNext()
          that.lookahead = that.popNext()
        } else {
          equal = this.lookahead.value == that.lookahead.value
          this.lookahead =  this.findLeftMostOrPopOnEmpty(this.goRight(this.lookahead))
          that.lookahead =  that.findLeftMostOrPopOnEmpty(that.goRight(that.lookahead))
        }
      }
      equal && (this.lookahead eq null) && (that.lookahead eq null)
    }
    def sameEntries[X](that:EqualsIterator[A,X]): Boolean = {
      var equal = true
      while (equal && (this.lookahead ne null) && (that.lookahead ne null)) {
        if (this.lookahead eq that.lookahead) {
          this.lookahead = this.popNext()
          that.lookahead = that.popNext()
        } else {
          equal = ((this.lookahead.key.asInstanceOf[AnyRef] eq that.lookahead.key.asInstanceOf[AnyRef]) ||
            ordering.equiv(this.lookahead.key, that.lookahead.key)) && this.lookahead.value == that.lookahead.value
          this.lookahead =  this.findLeftMostOrPopOnEmpty(this.goRight(this.lookahead))
          that.lookahead =  that.findLeftMostOrPopOnEmpty(that.goRight(that.lookahead))
        }
      }
      equal && (this.lookahead eq null) && (that.lookahead eq null)
    }
  }
  private[this] class EntriesIterator[A: Ordering, B](tree: Tree[A, B], focus: Option[A]) extends TreeIterator[A, B, (A, B)](tree, focus) {
    override def nextResult(tree: Tree[A, B]) = (tree.key, tree.value)
  }

  private[this] class KeysIterator[A: Ordering, B](tree: Tree[A, B], focus: Option[A]) extends TreeIterator[A, B, A](tree, focus) {
    override def nextResult(tree: Tree[A, B]) = tree.key
  }

  private[this] class ValuesIterator[A: Ordering, B](tree: Tree[A, B], focus: Option[A]) extends TreeIterator[A, B, B](tree, focus) {
    override def nextResult(tree: Tree[A, B]) = tree.value
  }

  /** Build a Tree suitable for a TreeSet from an ordered sequence of keys */
  def fromOrderedKeys[A](xs: Iterator[A], size: Int): Tree[A, Null] = {
    val maxUsedDepth = 32 - Integer.numberOfLeadingZeros(size) // maximum depth of non-leaf nodes
    def f(level: Int, size: Int): Tree[A, Null] = size match {
      case 0 => null
      case 1 => mkTree(level != maxUsedDepth || level == 1, xs.next(), null, null, null)
      case n =>
        val leftSize = (size-1)/2
        val left = f(level+1, leftSize)
        val x = xs.next()
        val right = f(level+1, size-1-leftSize)
        BlackTree(x, null, left, right)
    }
    f(1, size)
  }

  /** Build a Tree suitable for a TreeMap from an ordered sequence of key/value pairs */
  def fromOrderedEntries[A, B](xs: Iterator[(A, B)], size: Int): Tree[A, B] = {
    val maxUsedDepth = 32 - Integer.numberOfLeadingZeros(size) // maximum depth of non-leaf nodes
    def f(level: Int, size: Int): Tree[A, B] = size match {
      case 0 => null
      case 1 =>
        val (k, v) = xs.next()
        mkTree(level != maxUsedDepth || level == 1, k, v, null, null)
      case n =>
        val leftSize = (size-1)/2
        val left = f(level+1, leftSize)
        val (k, v) = xs.next()
        val right = f(level+1, size-1-leftSize)
        BlackTree(k, v, left, right)
    }
    f(1, size)
  }

  def transform[A, B, C](t: Tree[A, B], f: (A, B) => C): Tree[A, C] =
    if(t eq null) null
    else {
      val k = t.key
      val v = t.value
      val l = t.left
      val r = t.right
      val l2 = transform(l, f)
      val v2 = f(k, v)
      val r2 = transform(r, f)
      if((v2.asInstanceOf[AnyRef] eq v.asInstanceOf[AnyRef])
          && (l2 eq l)
          && (r2 eq r)) t.asInstanceOf[Tree[A, C]]
      else mkTree(t.isBlack, k, v2, l2, r2)
    }

  def filterEntries[A, B](t: Tree[A, B], f: (A, B) => Boolean): Tree[A, B] = if(t eq null) null else {
    def fk(t: Tree[A, B]): Tree[A, B] = {
      val k = t.key
      val v = t.value
      val l = t.left
      val r = t.right
      val l2 = if(l eq null) null else fk(l)
      val keep = f(k, v)
      val r2 = if(r eq null) null else fk(r)
      if(!keep) join2(l2, r2)
      else if((l2 eq l) && (r2 eq r)) t
      else join(l2, k, v, r2)
    }
    blacken(fk(t))
  }

  private[this] val null2 = (null, null)

  def partitionEntries[A, B](t: Tree[A, B], p: (A, B) => Boolean): (Tree[A, B], Tree[A, B]) = if(t eq null) (null, null) else {
    if (t eq null) null2
    else {
      object partitioner {
        var tmpk, tmpd = null: Tree[A, B] // shared vars to avoid returning tuples from fk
        def fk(t: Tree[A, B]): Unit = {
          val k                  = t.key
          val v                  = t.value
          val l                  = t.left
          val r                  = t.right
          var l2k, l2d, r2k, r2d = null: Tree[A, B]
          if (l ne null) {
            fk(l)
            l2k = tmpk
            l2d = tmpd
          }
          val keep = p(k, v)
          if (r ne null) {
            fk(r)
            r2k = tmpk
            r2d = tmpd
          }
          val jk =
            if (!keep) join2(l2k, r2k)
            else if ((l2k eq l) && (r2k eq r)) t
                 else join(l2k, k, v, r2k)
          val jd =
            if (keep) join2(l2d, r2d)
            else if ((l2d eq l) && (r2d eq r)) t
                 else join(l2d, k, v, r2d)
          tmpk = jk
          tmpd = jd
        }
      }

      partitioner.fk(t)
      (blacken(partitioner.tmpk), blacken(partitioner.tmpd))
    }
  }

  // Based on Stefan Kahrs' Haskell version of Okasaki's Red&Black Trees
  // Constructing Red-Black Trees, Ralf Hinze: [[https://www.cs.ox.ac.uk/ralf.hinze/publications/WAAAPL99b.ps.gz]]
  // Red-Black Trees in a Functional Setting, Chris Okasaki: [[https://wiki.rice.edu/confluence/download/attachments/2761212/Okasaki-Red-Black.pdf]] */

  private[this] def del[A, B](tree: Tree[A, B], k: A)(implicit ordering: Ordering[A]): Tree[A, B] = if (tree eq null) null else {
    val cmp = ordering.compare(k, tree.key)
    if (cmp < 0) {
      val newLeft = del(tree.left, k)
      if (newLeft eq tree.left) tree
      else if (isBlackTree(tree.left)) balLeft(tree, newLeft, tree.right)
      else tree.redWithLeft(newLeft)
    } else if (cmp > 0) {
      val newRight = del(tree.right, k)
      if (newRight eq tree.right) tree
      else if (isBlackTree(tree.right)) balRight(tree, tree.left, newRight)
      else tree.redWithRight(newRight)
    } else append(tree.left, tree.right)
  }

  private[this] def balance[A, B](tree: Tree[A,B], tl: Tree[A, B], tr: Tree[A, B]): Tree[A, B] =
    if (isRedTree(tl)) {
      if (isRedTree(tr)) tree.redWithLeftRight(tl.black, tr.black)
      else if (isRedTree(tl.left))  tl.withLeftRight(tl.left.black, tree.blackWithLeftRight(tl.right, tr))
      else if (isRedTree(tl.right)) tl.right.withLeftRight(tl.blackWithRight(tl.right.left), tree.blackWithLeftRight(tl.right.right, tr))
      else tree.blackWithLeftRight(tl, tr)
    } else if (isRedTree(tr)) {
      if (isRedTree(tr.right))     tr.withLeftRight(tree.blackWithLeftRight(tl, tr.left), tr.right.black)
      else if (isRedTree(tr.left)) tr.left.withLeftRight(tree.blackWithLeftRight(tl, tr.left.left), tr.blackWithLeftRight(tr.left.right, tr.right))
      else tree.blackWithLeftRight(tl, tr)
    } else tree.blackWithLeftRight(tl, tr)

  private[this] def balLeft[A, B](tree: Tree[A,B], tl: Tree[A, B], tr: Tree[A, B]): Tree[A, B] =
    if (isRedTree(tl)) tree.redWithLeftRight(tl.black, tr)
    else if (isBlackTree(tr)) balance(tree, tl, tr.red)
    else if (isRedTree(tr) && isBlackTree(tr.left))
         tr.left.redWithLeftRight(tree.blackWithLeftRight(tl, tr.left.left), balance(tr, tr.left.right, tr.right.red))
    else sys.error("Defect: invariance violation")

  private[this] def balRight[A, B](tree: Tree[A,B], tl: Tree[A, B], tr: Tree[A, B]): Tree[A, B] =
    if (isRedTree(tr)) tree.redWithLeftRight(tl, tr.black)
    else if (isBlackTree(tl)) balance(tree, tl.red, tr)
    else if (isRedTree(tl) && isBlackTree(tl.right))
         tl.right.redWithLeftRight(balance(tl, tl.left.red, tl.right.left), tree.blackWithLeftRight(tl.right.right, tr))
    else sys.error("Defect: invariance violation")

  /** `append` is similar to `join2` but requires that both subtrees have the same black height */
  private[this] def append[A, B](tl: Tree[A, B], tr: Tree[A, B]): Tree[A, B] = {
    if (tl eq null) tr
    else if (tr eq null) tl
    else if (tl.isRed) {
           if (tr.isRed) {
             //tl is red, tr is red
             val bc = append(tl.right, tr.left)
             if (isRedTree(bc)) bc.withLeftRight(tl.withRight(bc.left), tr.withLeft(bc.right))
             else tl.withRight(tr.withLeft(bc))
           } else {
             //tl is red, tr is black
             tl.withRight(append(tl.right, tr))
           }
         } else {
           if (tr.isBlack) {
             //tl is black tr is black
             val bc = append(tl.right, tr.left)
             if (isRedTree(bc)) bc.withLeftRight(tl.withRight(bc.left), tr.withLeft(bc.right))
             else balLeft(tl, tl.left, tr.withLeft(bc))
           } else {
             //tl is black tr is red
             tr.withLeft(append(tl, tr.left))
           }
         }
  }


  // Bulk operations based on "Just Join for Parallel Ordered Sets" (https://www.cs.cmu.edu/~guyb/papers/BFS16.pdf)
  // We don't store the black height in the tree so we pass it down into the join methods and derive the black height
  // of child nodes from it. Where possible the black height is used directly instead of deriving the rank from it.
  // Our trees are supposed to have a black root so we always blacken as the last step of union/intersect/difference.

  def union[A, B](t1: Tree[A, B], t2: Tree[A, B])(implicit ordering: Ordering[A]): Tree[A, B] = blacken(_union(t1, t2))

  def intersect[A, B](t1: Tree[A, B], t2: Tree[A, B])(implicit ordering: Ordering[A]): Tree[A, B] = blacken(_intersect(t1, t2))

  def difference[A, B](t1: Tree[A, B], t2: Tree[A, _])(implicit ordering: Ordering[A]): Tree[A, B] =
    blacken(_difference(t1, t2.asInstanceOf[Tree[A, B]]))

  /** Compute the rank from a tree and its black height */
  @`inline` private[this] def rank(t: Tree[_, _], bh: Int): Int = {
    if(t eq null) 0
    else if(t.isBlack) 2*(bh-1)
    else 2*bh-1
  }

  private[this] def joinRight[A, B](tl: Tree[A, B], k: A, v: B, tr: Tree[A, B], bhtl: Int, rtr: Int): Tree[A, B] = {
    val rtl = rank(tl, bhtl)
    if(rtl == (rtr/2)*2) RedTree(k, v, tl, tr)
    else {
      val tlBlack = isBlackTree(tl)
      val bhtlr = if(tlBlack) bhtl-1 else bhtl
      val ttr = joinRight(tl.right, k, v, tr, bhtlr, rtr)
      if(tlBlack && isRedTree(ttr) && isRedTree(ttr.right))
        RedTree(ttr.key, ttr.value,
          BlackTree(tl.key, tl.value, tl.left, ttr.left),
          ttr.right.black)
      else mkTree(tlBlack, tl.key, tl.value, tl.left, ttr)
    }
  }

  private[this] def joinLeft[A, B](tl: Tree[A, B], k: A, v: B, tr: Tree[A, B], rtl: Int, bhtr: Int): Tree[A, B] = {
    val rtr = rank(tr, bhtr)
    if(rtr == (rtl/2)*2) RedTree(k, v, tl, tr)
    else {
      val trBlack = isBlackTree(tr)
      val bhtrl = if(trBlack) bhtr-1 else bhtr
      val ttl = joinLeft(tl, k, v, tr.left, rtl, bhtrl)
      if(trBlack && isRedTree(ttl) && isRedTree(ttl.left))
        RedTree(ttl.key, ttl.value,
          ttl.left.black,
          BlackTree(tr.key, tr.value, ttl.right, tr.right))
      else mkTree(trBlack, tr.key, tr.value, ttl, tr.right)
    }
  }

  private[this] def join[A, B](tl: Tree[A, B], k: A, v: B, tr: Tree[A, B]): Tree[A, B] = {
    @tailrec def h(t: Tree[_, _], i: Int): Int =
      if(t eq null) i+1 else h(t.left, if(t.isBlack) i+1 else i)
    val bhtl = h(tl, 0)
    val bhtr = h(tr, 0)
    if(bhtl > bhtr) {
      val tt = joinRight(tl, k, v, tr, bhtl, rank(tr, bhtr))
      if(isRedTree(tt) && isRedTree(tt.right)) tt.black
      else tt
    } else if(bhtr > bhtl) {
      val tt = joinLeft(tl, k, v, tr, rank(tl, bhtl), bhtr)
      if(isRedTree(tt) && isRedTree(tt.left)) tt.black
      else tt
    } else mkTree(isRedTree(tl) || isRedTree(tr), k, v, tl, tr)
  }

  private[this] def split[A, B](t: Tree[A, B], k2: A)(implicit ordering: Ordering[A]): (Tree[A, B], Tree[A, B], Tree[A, B], A) =
    if(t eq null) (null, null, null, k2)
    else {
      val cmp = ordering.compare(k2, t.key)
      if(cmp == 0) (t.left, t, t.right, t.key)
      else if(cmp < 0) {
        val (ll, b, lr, k1) = split(t.left, k2)
        (ll, b, join(lr, t.key, t.value, t.right), k1)
      } else {
        val (rl, b, rr, k1) = split(t.right, k2)
        (join(t.left, t.key, t.value, rl), b, rr, k1)
      }
    }

  private[this] def splitLast[A, B](t: Tree[A, B]): (Tree[A, B], A, B) =
    if(t.right eq null) (t.left, t.key, t.value)
    else {
      val (tt, kk, vv) = splitLast(t.right)
      (join(t.left, t.key, t.value, tt), kk, vv)
    }

  private[this] def join2[A, B](tl: Tree[A, B], tr: Tree[A, B]): Tree[A, B] =
    if(tl eq null) tr
    else if(tr eq null) tl
    else {
      val (ttl, k, v) = splitLast(tl)
      join(ttl, k, v, tr)
    }

  private[this] def _union[A, B](t1: Tree[A, B], t2: Tree[A, B])(implicit ordering: Ordering[A]): Tree[A, B] =
    if((t1 eq null) || (t1 eq t2)) t2
    else if(t2 eq null) t1
    else {
      val (l1, _, r1, k1) = split(t1, t2.key)
      val tl = _union(l1, t2.left)
      val tr = _union(r1, t2.right)
      join(tl, k1, t2.value, tr)
    }

  private[this] def _intersect[A, B](t1: Tree[A, B], t2: Tree[A, B])(implicit ordering: Ordering[A]): Tree[A, B] =
    if((t1 eq null) || (t2 eq null)) null
    else if (t1 eq t2) t1
    else {
      val (l1, b, r1, k1) = split(t1, t2.key)
      val tl = _intersect(l1, t2.left)
      val tr = _intersect(r1, t2.right)
      if(b ne null) join(tl, k1, t2.value, tr)
      else join2(tl, tr)
    }

  private[this] def _difference[A, B](t1: Tree[A, B], t2: Tree[A, B])(implicit ordering: Ordering[A]): Tree[A, B] =
    if((t1 eq null) || (t2 eq null)) t1
    else if (t1 eq t2) null
    else {
      val (l1, _, r1, _) = split(t1, t2.key)
      val tl = _difference(l1, t2.left)
      val tr = _difference(r1, t2.right)
      join2(tl, tr)
    }
}
