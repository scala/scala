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

/** This annotation indicates that bean information should
 *  <strong>not</strong> be generated for the val, var, or def that it is
 *  attached to.
 *
 *  @author Ross Judson (rjudson@managedobjects.com)
 */
@deprecated(message = "the generation of BeanInfo classes is no longer supported", since = "2.12.5")
class BeanInfoSkip extends scala.annotation.Annotation
