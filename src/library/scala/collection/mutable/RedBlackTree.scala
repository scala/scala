package scala.collection.mutable

import scala.annotation.tailrec
import scala.collection.Iterator

/**
 * An object containing the red-black tree implementation used by mutable `TreeMaps`.
 *
 * The trees implemented in this object are *not* thread safe.
 *
 * @author Rui Gonçalves
 * @version 2.12
 * @since 2.12
 */
private[collection] object RedBlackTree {

  // ---- class structure ----

  // For performance reasons, this implementation uses `null` references to represent leaves instead of a sentinel node.
  // Currently, the internal nodes do not store their subtree size - only the tree object keeps track of their size.
  // Therefore, while obtaining the size of the whole tree is O(1), knowing the number of entries inside a range is O(n)
  // on the size of the range.

  @SerialVersionUID(21575944040195605L)
  final class Tree[A, B](var root: Node[A, B], var size: Int) extends Serializable

  @SerialVersionUID(1950599696441054720L)
  final class Node[A, B](var key: A, var value: B, var red: Boolean,
                         var left: Node[A, B], var right: Node[A, B], var parent: Node[A, B]) extends Serializable {

    override def toString: String = "Node(" + key + ", " + value + ", " + red + ", " + left + ", " + right + ")"
  }

  object Tree {
    def empty[A, B]: Tree[A, B] = new Tree(null, 0)
  }

  object Node {

    @inline def apply[A, B](key: A, value: B, red: Boolean,
                            left: Node[A, B], right: Node[A, B], parent: Node[A, B]): Node[A, B] =
      new Node(key, value, red, left, right, parent)

    @inline def leaf[A, B](key: A, value: B, red: Boolean, parent: Node[A, B]): Node[A, B] =
      new Node(key, value, red, null, null, parent)

    def unapply[A, B](t: Node[A, B]) = Some((t.key, t.value, t.left, t.right, t.parent))
  }

  // ---- getters ----

  def isRed(node: Node[_, _]) = (node ne null) && node.red
  def isBlack(node: Node[_, _]) = (node eq null) || !node.red

  // ---- size ----

  def size(node: Node[_, _]): Int = if (node eq null) 0 else 1 + size(node.left) + size(node.right)
  def size(tree: Tree[_, _]): Int = tree.size
  def isEmpty(tree: Tree[_, _]) = tree.root eq null
  def clear(tree: Tree[_, _]): Unit = { tree.root = null; tree.size = 0 }

  // ---- search ----

  def get[A: Ordering, B](tree: Tree[A, B], key: A): Option[B] = getNode(tree.root, key) match {
    case null => None
    case node => Some(node.value)
  }

  @tailrec private[this] def getNode[A, B](node: Node[A, B], key: A)(implicit ord: Ordering[A]): Node[A, B] =
    if (node eq null) null
    else {
      val cmp = ord.compare(key, node.key)
      if (cmp < 0) getNode(node.left, key)
      else if (cmp > 0) getNode(node.right, key)
      else node
    }

  def contains[A: Ordering](tree: Tree[A, _], key: A) = getNode(tree.root, key) ne null

  def min[A, B](tree: Tree[A, B]): Option[(A, B)] = minNode(tree.root) match {
    case null => None
    case node => Some((node.key, node.value))
  }

  def minKey[A](tree: Tree[A, _]): Option[A] = minNode(tree.root) match {
    case null => None
    case node => Some(node.key)
  }

  private def minNode[A, B](node: Node[A, B]): Node[A, B] =
    if (node eq null) null else minNodeNonNull(node)

  @tailrec def minNodeNonNull[A, B](node: Node[A, B]): Node[A, B] =
    if (node.left eq null) node else minNodeNonNull(node.left)

  def max[A, B](tree: Tree[A, B]): Option[(A, B)] = maxNode(tree.root) match {
    case null => None
    case node => Some((node.key, node.value))
  }

  def maxKey[A](tree: Tree[A, _]): Option[A] = maxNode(tree.root) match {
    case null => None
    case node => Some(node.key)
  }

  private def maxNode[A, B](node: Node[A, B]): Node[A, B] =
    if (node eq null) null else maxNodeNonNull(node)

  @tailrec def maxNodeNonNull[A, B](node: Node[A, B]): Node[A, B] =
    if (node.right eq null) node else maxNodeNonNull(node.right)

  /**
   * Returns the first (lowest) map entry with a key equal or greater than `key`. Returns `None` if there is no such
   * node.
   */
  def minAfter[A, B](tree: Tree[A, B], key: A)(implicit ord: Ordering[A]): Option[(A, B)] =
    minNodeAfter(tree.root, key) match {
      case null => None
      case node => Some((node.key, node.value))
    }

  def minKeyAfter[A](tree: Tree[A, _], key: A)(implicit ord: Ordering[A]): Option[A] =
    minNodeAfter(tree.root, key) match {
      case null => None
      case node => Some(node.key)
    }

  private[this] def minNodeAfter[A, B](node: Node[A, B], key: A)(implicit ord: Ordering[A]): Node[A, B] = {
    if (node eq null) null
    else {
      var y: Node[A, B] = null
      var x = node
      var cmp = 1
      while ((x ne null) && cmp != 0) {
        y = x
        cmp = ord.compare(key, x.key)
        x = if (cmp < 0) x.left else x.right
      }
      if (cmp <= 0) y else successor(y)
    }
  }

  /**
   * Returns the last (highest) map entry with a key smaller than `key`. Returns `None` if there is no such node.
   */
  def maxBefore[A, B](tree: Tree[A, B], key: A)(implicit ord: Ordering[A]): Option[(A, B)] =
    maxNodeBefore(tree.root, key) match {
      case null => None
      case node => Some((node.key, node.value))
    }

  def maxKeyBefore[A](tree: Tree[A, _], key: A)(implicit ord: Ordering[A]): Option[A] =
    maxNodeBefore(tree.root, key) match {
      case null => None
      case node => Some(node.key)
    }

  private[this] def maxNodeBefore[A, B](node: Node[A, B], key: A)(implicit ord: Ordering[A]): Node[A, B] = {
    if (node eq null) null
    else {
      var y: Node[A, B] = null
      var x = node
      var cmp = 1
      while ((x ne null) && cmp != 0) {
        y = x
        cmp = ord.compare(key, x.key)
        x = if (cmp < 0) x.left else x.right
      }
      if (cmp > 0) y else predecessor(y)
    }
  }

  // ---- insertion ----

  def insert[A, B](tree: Tree[A, B], key: A, value: B)(implicit ord: Ordering[A]): Unit = {
    var y: Node[A, B] = null
    var x = tree.root
    var cmp = 1
    while ((x ne null) && cmp != 0) {
      y = x
      cmp = ord.compare(key, x.key)
      x = if (cmp < 0) x.left else x.right
    }

    if (cmp == 0) y.value = value
    else {
      val z = Node.leaf(key, value, red = true, y)

      if (y eq null) tree.root = z
      else if (cmp < 0) y.left = z
      else y.right = z

      fixAfterInsert(tree, z)
      tree.size += 1
    }
  }

  private[this] def fixAfterInsert[A, B](tree: Tree[A, B], node: Node[A, B]): Unit = {
    var z = node
    while (isRed(z.parent)) {
      if (z.parent eq z.parent.parent.left) {
        val y = z.parent.parent.right
        if (isRed(y)) {
          z.parent.red = false
          y.red = false
          z.parent.parent.red = true
          z = z.parent.parent
        } else {
          if (z eq z.parent.right) {
            z = z.parent
            rotateLeft(tree, z)
          }
          z.parent.red = false
          z.parent.parent.red = true
          rotateRight(tree, z.parent.parent)
        }
      } else { // symmetric cases
        val y = z.parent.parent.left
        if (isRed(y)) {
          z.parent.red = false
          y.red = false
          z.parent.parent.red = true
          z = z.parent.parent
        } else {
          if (z eq z.parent.left) {
            z = z.parent
            rotateRight(tree, z)
          }
          z.parent.red = false
          z.parent.parent.red = true
          rotateLeft(tree, z.parent.parent)
        }
      }
    }
    tree.root.red = false
  }

  // ---- deletion ----

  def delete[A, B](tree: Tree[A, B], key: A)(implicit ord: Ordering[A]): Unit = {
    val z = getNode(tree.root, key)
    if (z ne null) {
      var y = z
      var yIsRed = y.red
      var x: Node[A, B] = null
      var xParent: Node[A, B] = null

      if (z.left eq null) {
        x = z.right
        transplant(tree, z, z.right)
        xParent = z.parent
      }
      else if (z.right eq null) {
        x = z.left
        transplant(tree, z, z.left)
        xParent = z.parent
      }
      else {
        y = minNodeNonNull(z.right)
        yIsRed = y.red
        x = y.right

        if (y.parent eq z) xParent = y
        else {
          xParent = y.parent
          transplant(tree, y, y.right)
          y.right = z.right
          y.right.parent = y
        }
        transplant(tree, z, y)
        y.left = z.left
        y.left.parent = y
        y.red = z.red
      }

      if (!yIsRed) fixAfterDelete(tree, x, xParent)
      tree.size -= 1
    }
  }

  private[this] def fixAfterDelete[A, B](tree: Tree[A, B], node: Node[A, B], parent: Node[A, B]): Unit = {
    var x = node
    var xParent = parent
    while ((x ne tree.root) && isBlack(x)) {
      if (x eq xParent.left) {
        var w = xParent.right
        // assert(w ne null)

        if (w.red) {
          w.red = false
          xParent.red = true
          rotateLeft(tree, xParent)
          w = xParent.right
        }
        if (isBlack(w.left) && isBlack(w.right)) {
          w.red = true
          x = xParent
        } else {
          if (isBlack(w.right)) {
            w.left.red = false
            w.red = true
            rotateRight(tree, w)
            w = xParent.right
          }
          w.red = xParent.red
          xParent.red = false
          w.right.red = false
          rotateLeft(tree, xParent)
          x = tree.root
        }
      } else { // symmetric cases
        var w = xParent.left
        // assert(w ne null)

        if (w.red) {
          w.red = false
          xParent.red = true
          rotateRight(tree, xParent)
          w = xParent.left
        }
        if (isBlack(w.right) && isBlack(w.left)) {
          w.red = true
          x = xParent
        } else {
          if (isBlack(w.left)) {
            w.right.red = false
            w.red = true
            rotateLeft(tree, w)
            w = xParent.left
          }
          w.red = xParent.red
          xParent.red = false
          w.left.red = false
          rotateRight(tree, xParent)
          x = tree.root
        }
      }
      xParent = x.parent
    }
    if (x ne null) x.red = false
  }

  // ---- helpers ----

  /**
   * Returns the node that follows `node` in an in-order tree traversal. If `node` has the maximum key (and is,
   * therefore, the last node), this method returns `null`.
   */
  private[this] def successor[A, B](node: Node[A, B]): Node[A, B] = {
    if (node.right ne null) minNodeNonNull(node.right)
    else {
      var x = node
      var y = x.parent
      while ((y ne null) && (x eq y.right)) {
        x = y
        y = y.parent
      }
      y
    }
  }

  /**
   * Returns the node that precedes `node` in an in-order tree traversal. If `node` has the minimum key (and is,
   * therefore, the first node), this method returns `null`.
   */
  private[this] def predecessor[A, B](node: Node[A, B]): Node[A, B] = {
    if (node.left ne null) maxNodeNonNull(node.left)
    else {
      var x = node
      var y = x.parent
      while ((y ne null) && (x eq y.left)) {
        x = y
        y = y.parent
      }
      y
    }
  }

  private[this] def rotateLeft[A, B](tree: Tree[A, B], x: Node[A, B]): Unit = if (x ne null) {
    // assert(x.right ne null)
    val y = x.right
    x.right = y.left

    if (y.left ne null) y.left.parent = x
    y.parent = x.parent

    if (x.parent eq null) tree.root = y
    else if (x eq x.parent.left) x.parent.left = y
    else x.parent.right = y

    y.left = x
    x.parent = y
  }

  private[this] def rotateRight[A, B](tree: Tree[A, B], x: Node[A, B]): Unit = if (x ne null) {
    // assert(x.left ne null)
    val y = x.left
    x.left = y.right

    if (y.right ne null) y.right.parent = x
    y.parent = x.parent

    if (x.parent eq null) tree.root = y
    else if (x eq x.parent.right) x.parent.right = y
    else x.parent.left = y

    y.right = x
    x.parent = y
  }

  /**
   * Transplant the node `from` to the place of node `to`. This is done by setting `from` as a child of `to`'s previous
   * parent and setting `from`'s parent to the `to`'s previous parent. The children of `from` are left unchanged.
   */
  private[this] def transplant[A, B](tree: Tree[A, B], to: Node[A, B], from: Node[A, B]): Unit = {
    if (to.parent eq null) tree.root = from
    else if (to eq to.parent.left) to.parent.left = from
    else to.parent.right = from

    if (from ne null) from.parent = to.parent
  }

  // ---- tree traversal ----

  def foreach[A, B, U](tree: Tree[A, B], f: ((A, B)) => U): Unit = foreachNode(tree.root, f)

  private[this] def foreachNode[A, B, U](node: Node[A, B], f: ((A, B)) => U): Unit =
    if (node ne null) foreachNodeNonNull(node, f)

  private[this] def foreachNodeNonNull[A, B, U](node: Node[A, B], f: ((A, B)) => U): Unit = {
    if (node.left ne null) foreachNodeNonNull(node.left, f)
    f((node.key, node.value))
    if (node.right ne null) foreachNodeNonNull(node.right, f)
  }

  def foreachKey[A, U](tree: Tree[A, _], f: A => U): Unit = foreachNodeKey(tree.root, f)

  private[this] def foreachNodeKey[A, U](node: Node[A, _], f: A => U): Unit =
    if (node ne null) foreachNodeKeyNonNull(node, f)

  private[this] def foreachNodeKeyNonNull[A, U](node: Node[A, _], f: A => U): Unit = {
    if (node.left ne null) foreachNodeKeyNonNull(node.left, f)
    f(node.key)
    if (node.right ne null) foreachNodeKeyNonNull(node.right, f)
  }

  def transform[A, B](tree: Tree[A, B], f: (A, B) => B): Unit = transformNode(tree.root, f)

  private[this] def transformNode[A, B, U](node: Node[A, B], f: (A, B) => B): Unit =
    if (node ne null) transformNodeNonNull(node, f)

  private[this] def transformNodeNonNull[A, B, U](node: Node[A, B], f: (A, B) => B): Unit = {
    if (node.left ne null) transformNodeNonNull(node.left, f)
    node.value = f(node.key, node.value)
    if (node.right ne null) transformNodeNonNull(node.right, f)
  }

  def iterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None, end: Option[A] = None): Iterator[(A, B)] =
    new EntriesIterator(tree, start, end)

  def keysIterator[A: Ordering](tree: Tree[A, _], start: Option[A] = None, end: Option[A] = None): Iterator[A] =
    new KeysIterator(tree, start, end)

  def valuesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A] = None, end: Option[A] = None): Iterator[B] =
    new ValuesIterator(tree, start, end)

  private[this] abstract class TreeIterator[A, B, R](tree: Tree[A, B], start: Option[A], end: Option[A])
                                                    (implicit ord: Ordering[A]) extends Iterator[R] {

    protected[this] def nextResult(node: Node[A, B]): R

    def hasNext: Boolean = nextNode ne null

    def next(): R = nextNode match {
      case null => throw new NoSuchElementException("next on empty iterator")
      case node =>
        nextNode = successor(node)
        setNullIfAfterEnd()
        nextResult(node)
    }

    private[this] var nextNode: Node[A, B] = start match {
      case None => minNode(tree.root)
      case Some(from) => minNodeAfter(tree.root, from)
    }

    private[this] def setNullIfAfterEnd(): Unit =
      if (end.isDefined && (nextNode ne null) && ord.compare(nextNode.key, end.get) >= 0)
        nextNode = null

    setNullIfAfterEnd()
  }

  private[this] final class EntriesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A], end: Option[A])
    extends TreeIterator[A, B, (A, B)](tree, start, end) {

    def nextResult(node: Node[A, B]) = (node.key, node.value)
  }

  private[this] final class KeysIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A], end: Option[A])
    extends TreeIterator[A, B, A](tree, start, end) {

    def nextResult(node: Node[A, B]) = node.key
  }

  private[this] final class ValuesIterator[A: Ordering, B](tree: Tree[A, B], start: Option[A], end: Option[A])
    extends TreeIterator[A, B, B](tree, start, end) {

    def nextResult(node: Node[A, B]) = node.value
  }

  // ---- debugging ----

  /**
   * Checks if the tree is in a valid state. That happens if:
   * - It is a valid binary search tree;
   * - All red-black properties are satisfied;
   * - All non-null nodes have their `parent` reference correct;
   * - The size variable in `tree` corresponds to the actual size of the tree.
   */
  def isValid[A: Ordering, B](tree: Tree[A, B]): Boolean =
    isValidBST(tree.root) && hasProperParentRefs(tree) && isValidRedBlackTree(tree) && size(tree.root) == tree.size

  /**
   * Returns true if all non-null nodes have their `parent` reference correct.
   */
  private[this] def hasProperParentRefs[A, B](tree: Tree[A, B]): Boolean = {

    def hasProperParentRefs(node: Node[A, B]): Boolean = {
      if (node eq null) true
      else {
        if ((node.left ne null) && (node.left.parent ne node) ||
          (node.right ne null) && (node.right.parent ne node)) false
        else hasProperParentRefs(node.left) && hasProperParentRefs(node.right)
      }
    }

    if(tree.root eq null) true
    else (tree.root.parent eq null) && hasProperParentRefs(tree.root)
  }

  /**
   * Returns true if this node follows the properties of a binary search tree.
   */
  private[this] def isValidBST[A, B](node: Node[A, B])(implicit ord: Ordering[A]): Boolean = {
    if (node eq null) true
    else {
      if ((node.left ne null) && (ord.compare(node.key, node.left.key) <= 0) ||
        (node.right ne null) && (ord.compare(node.key, node.right.key) >= 0)) false
      else isValidBST(node.left) && isValidBST(node.right)
    }
  }

  /**
   * Returns true if the tree has all the red-black tree properties: if the root node is black, if all children of red
   * nodes are black and if the path from any node to any of its null children has the same number of black nodes.
   */
  private[this] def isValidRedBlackTree[A, B](tree: Tree[A, B]): Boolean = {

    def noRedAfterRed(node: Node[A, B]): Boolean = {
      if (node eq null) true
      else if (node.red && (isRed(node.left) || isRed(node.right))) false
      else noRedAfterRed(node.left) && noRedAfterRed(node.right)
    }

    def blackHeight(node: Node[A, B]): Int = {
      if (node eq null) 1
      else {
        val lh = blackHeight(node.left)
        val rh = blackHeight(node.right)

        if (lh == -1 || lh != rh) -1
        else if (isRed(node)) lh
        else lh + 1
      }
    }

    isBlack(tree.root) && noRedAfterRed(tree.root) && blackHeight(tree.root) >= 0
  }
}
