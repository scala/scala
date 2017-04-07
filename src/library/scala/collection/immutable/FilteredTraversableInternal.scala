/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package immutable

import scala.annotation.tailrec

/**
  * Optimised filter functions for List
  * n.b. this is an internal class to help maintain compatibility and should not be used directly.
  */
private[immutable] trait FilteredTraversableInternal[+A, +Repr <: AnyRef with TraversableLike[A, Repr]] extends TraversableLike[A, Repr] {

  // Optimized for List

  override def filter(p: A => Boolean): Self = filterImpl(p, isFlipped = false)

  override def filterNot(p: A => Boolean): Self = filterImpl(p, isFlipped = true)

  private[this] def filterImpl(p: A => Boolean, isFlipped: Boolean): Self = {

    // everything seen so far so far is not included
    @tailrec def noneIn(l: Repr): Repr = {
      if (l.isEmpty)
        Nil.asInstanceOf[Repr]
      else {
        val h = l.head
        val t = l.tail
        if (p(h) != isFlipped)
          allIn(l, t)
        else
          noneIn(t)
      }
    }

    // everything from 'start' is included, if everything from this point is in we can return the origin
    // start otherwise if we discover an element that is out we must create a new partial list.
    @tailrec def allIn(start: Repr, remaining: Repr): Repr = {
      if (remaining.isEmpty)
        start
      else {
        val x = remaining.head
        if (p(x) != isFlipped)
          allIn(start, remaining.tail)
        else
          partialFill(start, remaining)
      }
    }

    // we have seen elements that should be included then one that should be excluded, start building
    def partialFill(origStart: Repr, firstMiss: Repr): Repr = {
      val newHead = new ::(origStart.head, Nil)
      var toProcess = origStart.tail
      var currentLast = newHead

      // we know that all elements are :: until at least firstMiss.tail
      while (!(toProcess eq firstMiss)) {
        val newElem = new ::(toProcess.head, Nil)
        currentLast.tl = newElem
        currentLast = newElem
        toProcess = toProcess.tail
      }

      // at this point newHead points to a list which is a duplicate of all the 'in' elements up to the first miss.
      // currentLast is the last element in that list.

      // now we are going to try and share as much of the tail as we can, only moving elements across when we have to.
      var next = firstMiss.tail
      var nextToCopy = next // the next element we would need to copy to our list if we cant share.
      while (!next.isEmpty) {
        // generally recommended is next.isNonEmpty but this incurs an extra method call.
        val head: A = next.head
        if (p(head) != isFlipped) {
          next = next.tail
        } else {
          // its not a match - do we have outstanding elements?
          while (!(nextToCopy eq next)) {
            val newElem = new ::(nextToCopy.head, Nil)
            currentLast.tl = newElem
            currentLast = newElem
            nextToCopy = nextToCopy.tail
          }
          nextToCopy = next.tail
          next = next.tail
        }
      }

      // we have remaining elements - they are unchanged attach them to the end
      if (!nextToCopy.isEmpty)
        currentLast.tl = nextToCopy.asInstanceOf[List[A]]

      newHead.asInstanceOf[Repr]
    }

    noneIn(repr)
  }
}