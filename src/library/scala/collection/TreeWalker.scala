/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection

/**
 *  @author Sean McDirmid
 */
object TreeWalker {

  object Empty extends TreeWalker[Nothing] {
    def hasNext = None
    def next = throw new NoSuchElementException
  }

  private case class NonEmpty[+A](item: A, right: () => TreeWalker[A]) extends TreeWalker[A] {
    private var spent = false
    def hasNext = if (spent) right().hasNext else Some(this)
    def next = if (spent) right().next else {
      spent = true
      item
    }
  }

  def apply[A](item: A): TreeWalker[A] = NonEmpty(item, () => Empty)

  def apply[A](item: A, right: () => TreeWalker[A]): () => TreeWalker[A] =
    () => NonEmpty(item, right)

  def apply[A](left: TreeWalker[A], item: A, right: () => TreeWalker[A]): TreeWalker[A] =
    left match {
      case Empty =>
        NonEmpty(item, right)
      case NonEmpty(first, middle) =>
        val rest = NonEmpty(item,right)
        NonEmpty(first, apply(middle, () => rest))
    }

  def apply[A](left: () => TreeWalker[A], right: () => TreeWalker[A]): () => TreeWalker[A] =
    () => (left() match {
      case Empty => right()
      case NonEmpty(item, middle) =>
        NonEmpty(item, apply(middle, right))
    })
}

/**
 *  @author Sean McDirmid
 */
sealed abstract class TreeWalker[+A] {
  def hasNext : Option[TreeWalker[A]]
  def next: A
  def append[B >: A](item: B): TreeWalker[B] = append(item, () => TreeWalker.Empty);
  def append[B >: A](item: B, right: () => TreeWalker[B]): TreeWalker[B] = TreeWalker[B](this, item, right);
  def append[B >: A](right : () => TreeWalker[B]) = TreeWalker(() => this,right)();
  class Elements extends Iterator[A] {
    private var cursor : TreeWalker[Any] = TreeWalker.this;
    def check = {
      if (!(cursor eq null)) cursor.hasNext match {
      case None => cursor = null;
      case Some(c) if !(c eq cursor) =>
        cursor = c;
      case Some(_) =>
      }
      cursor;
    }
    def hasNext = !(check eq null);
    def next = check match {
      case null => throw new NoSuchElementException
      case cursor => cursor.next.asInstanceOf[A]
    }
  }
  def elements = new Elements
}




