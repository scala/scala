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

package scala.tools.partest

object Util {

  def prettyArray(a: Array[_]): collection.IndexedSeq[Any] = new collection.AbstractSeq[Any] with collection.IndexedSeq[Any] {
    def length = a.length

    def apply(idx: Int): Any = a(idx) match {
      case x: AnyRef if x.getClass.isArray => prettyArray(x.asInstanceOf[Array[_]])
      case x => x
    }

    override def className = "Array"
  }

  implicit class ArrayDeep(val a: Array[_]) extends AnyVal {
    def deep: collection.IndexedSeq[Any] = prettyArray(a)
  }
}
