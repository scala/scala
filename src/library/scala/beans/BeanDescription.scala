/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.beans

/** Provides a short description that will be included when generating
 *  bean information. This annotation can be attached to the bean itself,
 *  or to any member.
 *
 *  @author Ross Judson (rjudson@managedobjects.com)
 */
@deprecated(message = "the generation of BeanInfo classes is no longer supported", since = "2.12.5")
class BeanDescription(val description: String) extends scala.annotation.Annotation

