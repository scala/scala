/* NSC -- new Scala compiler -- Copyright 2007-2010 LAMP/EPFL */

package scala.tools.nsc
package doc
package model
package comment

import scala.collection._

/** A Scaladoc comment and all its tags.
  *
  * '''Note:''' the only instantiation site of this class is in `Parser`.
  *
  * @author Gilles Dubochet
  * @author Manohar Jonnalagedda */
abstract class Comment {

  /** */
  def body: Body
  /* author|deprecated|param|return|see|since|throws|version|todo|tparam */

  def short: Inline

  /** */
  def authors: List[Body]

  /** */
  def see: List[Body]

  /** */
  def result: Option[Body]

  /** */
  def throws: Map[String, Body]

  /** */
  def valueParams: Map[String, Body]

  /** */
  def typeParams: Map[String, Body]

  /** */
  def version: Option[Body]

  /** */
  def since: Option[Body]

  /** */
  def todo: List[Body]

  /** */
  def deprecated: Option[Body]

  override def toString =
    body.toString + "\n" +
    (authors map ("@author " + _.toString)).mkString("\n") +
    (result map ("@return " + _.toString)).mkString

}

