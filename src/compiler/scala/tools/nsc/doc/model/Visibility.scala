/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Gilles Dubochet
 */

package scala.tools.nsc
package doc
package model

/** An type that represents visibility of members. */
sealed trait Visibility {
  def isProtected: Boolean = false
  def isPublic: Boolean = false
}

/** The visibility of `private[this]` members. */
case class PrivateInInstance() extends Visibility

/** The visibility of `protected[this]` members. */
case class ProtectedInInstance() extends Visibility {
  override def isProtected = true
}

/** The visibility of `private[owner]` members. An unqualified private members
  * is encoded with `owner` equal to the members's `inTemplate`. */
case class PrivateInTemplate(owner: TemplateEntity) extends Visibility

/** The visibility of `protected[owner]` members. An unqualified protected
  * members is encoded with `owner` equal to the members's `inTemplate`.
  * Note that whilst the member is visible in any template owned by `owner`,
  * it is only visible in subclasses of the member's `inTemplate`. */
case class ProtectedInTemplate(owner: TemplateEntity) extends Visibility {
  override def isProtected = true
}

/** The visibility of public members. */
case class Public() extends Visibility {
  override def isPublic = true
}
