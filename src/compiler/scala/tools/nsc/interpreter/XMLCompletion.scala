/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

import xml.{ XML, Group, Node, NodeSeq }
import XMLCompletion._
import scala.collection.mutable.HashMap

class XMLCompletion(root: Node) extends CompletionAware {
  private val nodeCache = new HashMap[String, Node]
  private def getNode(s: String): Option[Node] = {
    completions // make sure cache is populated
    nodeCache get s
  }

  lazy val completions: List[String] = {
    def children = root.child.toList
    def uniqueTags = children groupBy (_.label) filter (_._2.size == 1) map (_._1)
    val uniqs = uniqueTags.toList

    children.foldLeft(List[String]())((res, node) => {
      val name = node.label
      def count = res filter (_ startsWith (name + "[")) size  // ]
      val suffix = if (uniqs contains name) "" else "[%d]" format (count + 1)
      val s = name + suffix

      nodeCache(s) = node

      s :: res
    }).sorted
  }

  override def execute(id: String)  = getNode(id)
  override def follow(id: String)   = getNode(id) map (x => new XMLCompletion(x))
}

object XMLCompletion {
  def apply(x: Node) = new XMLCompletion(x)
}
