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

package scala.tools.nsc
package doc
package base
package comment

import scala.collection.mutable.ListBuffer

/** A Scaladoc comment and all its tags.
  *
  * '''Note:''' the only instantiation site of this class is in [[model.CommentFactory]].
  *
  * @author Manohar Jonnalagedda
  * @author Gilles Dubochet */
abstract class Comment {

  /** The main body of the comment that describes what the entity does and is.  */
  def body: Body

  private def closeHtmlTags(inline: Inline): Inline = {
    val stack = ListBuffer.empty[HtmlTag]
    def scan(i: Inline): Unit = {
      i match {
        case Chain(list) =>
          list foreach scan
        case tag: HtmlTag => {
          if (stack.nonEmpty && tag.canClose(stack.last)) {
            stack.remove(stack.length-1)
          } else {
            tag.close match {
              case Some(t) =>
                stack += t
              case None =>
                ;
            }
          }
        }
        case _ =>
          ;
      }
    }
    scan(inline)
    Chain(List(inline) ++ stack.reverse)
  }

  /** A shorter version of the body. Either from `@shortDescription` or the
   *  first sentence of the body. */
  def short: Inline = {
    shortDescription orElse body.summary match {
      case Some(s) =>
        closeHtmlTags(s)
      case _ =>
        Text("")
    }
  }

  /** A list of authors. The empty list is used when no author is defined. */
  def authors: List[Body]

  /** A list of other resources to see, including links to other entities or
    * to external documentation. The empty list is used when no other resource
    * is mentioned. */
  def see: List[Body]

  /** A description of the result of the entity. Typically, this provides additional
    * information on the domain of the result, contractual post-conditions, etc. */
  def result: Option[Body]

  /** A map of exceptions that the entity can throw when accessed, and a
    * description of what they mean. */
  def throws: Map[String, Body]

  /** A map of value parameters, and a description of what they are. Typically,
    * this provides additional information on the domain of the parameters,
    * contractual pre-conditions, etc. */
  def valueParams: Map[String, Body]

  /** A map of type parameters, and a description of what they are. Typically,
    * this provides additional information on the domain of the parameters. */
  def typeParams: Map[String, Body]

  /** The version number of the entity. There is no formatting or further
    * meaning attached to this value. */
  def version: Option[Body]

  /** A version number of a containing entity where this member-entity was introduced. */
  def since: Option[Body]

  /** An annotation as to expected changes on this entity. */
  def todo: List[Body]

  /** Whether the entity is deprecated. Using the `@deprecated` Scala attribute
    * is preferable to using this Scaladoc tag. */
  def deprecated: Option[Body]

  /** An additional note concerning the contract of the entity. */
  def note: List[Body]

  /** A usage example related to the entity. */
  def example: List[Body]

  /** A description for the primary constructor */
  def constructor: Option[Body]

  /** A set of diagram directives for the inheritance diagram */
  def inheritDiagram: List[String]

  /** A set of diagram directives for the content diagram */
  def contentDiagram: List[String]

  /** The group this member is part of */
  def group: Option[String]

  /** Member group descriptions */
  def groupDesc: Map[String,Body]

  /** Member group names (overriding the short tag) */
  def groupNames: Map[String,String]

  /** Member group priorities */
  def groupPrio: Map[String,Int]

  /** A list of implicit conversions to hide */
  def hideImplicitConversions: List[String]

  /** A short description used in the entity-view and search results */
  def shortDescription: Option[Text]

  override def toString =
    body.toString + "\n" +
    (authors map ("@author " + _.toString)).mkString("\n") +
    (result map ("@return " + _.toString)).mkString("\n") +
    (version map ("@version " + _.toString)).mkString
}
