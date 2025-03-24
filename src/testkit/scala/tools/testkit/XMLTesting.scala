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

package scala.tools.testkit

object XMLTesting {
  object xml {
    val code = """
import collection.{immutable, mutable}, mutable.ArrayBuffer

package scala.xml {
  trait MetaData
    //def key: String
    //def value: Seq[Node]
    //def next: MetaData
  trait NamespaceBinding
  object TopScope extends NamespaceBinding
  object Null extends MetaData
  abstract class Node extends immutable.Seq[Node] {
    def label: String
    def child: Seq[Node] = Nil
    override def toString = label + child.mkString

    def iterator: Iterator[Node] = ??? // implements `def iterator: Iterator[A]`
    // Members declared in scala.collection.SeqOps
    def apply(i: Int): Node = ??? // implements `def apply(i: Int): A`
    def length: Int = ???
  }
  class Elem(prefix: String, val label: String, attributes1: MetaData, scope: NamespaceBinding, minimizeEmpty: Boolean, override val child: Node*) extends Node
  class NodeBuffer extends Seq[Node] {
    val nodes = ArrayBuffer.empty[Node]
    def &+(o: Any): this.type =
      o match {
        case n: Node => nodes.addOne(n); this
        case _ => throw new MatchError(o)
      }
    // Members declared in scala.collection.IterableOnce
    def iterator: Iterator[scala.xml.Node] = nodes.iterator
    // Members declared in scala.collection.SeqOps
    def apply(i: Int): scala.xml.Node = nodes(i)
    def length: Int = nodes.length
  }
  case class Text(text: String) extends Node {
    def label = text
  }
  case class Atom(t: Text) extends Node {
    def label = t.text
  }
  trait Attribute extends MetaData
  class PrefixedAttribute(pre: String, key: String, value: Seq[Node], next1: MetaData) extends Attribute
  class UnprefixedAttribute(key: String, value: Seq[Node], next1: MetaData) extends Attribute {
    def this(key: String, value: String, next1: MetaData) = this(key, Text(value), next1)
  }
}
"""
  }
}
