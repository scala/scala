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

package scala.collection


/** Buffered iterators are iterators which provide a method `head`
 *  that inspects the next element without discarding it.
 */
trait BufferedIterator[+A] extends Iterator[A] {

  /** Returns next element of iterator without advancing beyond it.
   */
  def head: A

  /** Returns an option of the next element of an iterator without advancing beyond it.
    * @return  the next element of this iterator if it has a next element
    *           `None` if it does not
    */
  def headOption : Option[A] = if (hasNext) Some(head) else None

  override def buffered: this.type = this
}
