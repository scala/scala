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
package model

/** An type that represents visibility of members. */
sealed trait Visibility {
  def isProtected: Boolean = false
  def isPublic: Boolean = false
  def isPrivate: Boolean = false
}

/** The visibility of `private[this]` members. */
case class PrivateInInstance() extends Visibility {
  override def isPrivate = true
}

/** The visibility of `protected[this]` members. */
case class ProtectedInInstance() extends Visibility {
  override def isProtected = true
}

/** The visibility of `private[owner]` members. An unqualified private members
  * is encoded with `owner` equal to the members's `inTemplate`. */
case class PrivateInTemplate(owner: Option[TypeEntity]) extends Visibility {
  override def isPrivate = true
}

/** The visibility of `protected[owner]` members. An unqualified protected
  * members is encoded with `owner` equal to `None`.
  * Note that whilst the member is visible in any template owned by `owner`,
  * it is only visible in subclasses of the member's `inTemplate`. */
case class ProtectedInTemplate(owner: Option[TypeEntity]) extends Visibility {
  override def isProtected = true
}

/** The visibility of public members. */
case class Public() extends Visibility {
  override def isPublic = true
}
