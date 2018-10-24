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

/** This annotation indicates that a JavaBean-compliant `BeanInfo` class
 *  should be generated for this annotated Scala class.
 *
 *  - A `'''val'''` becomes a read-only property.
 *  - A `'''var'''` becomes a read-write property.
 *  - A `'''def'''` becomes a method.
 *
 *  @author Ross Judson (rjudson@managedobjects.com)
 */
@deprecated(message = "the generation of BeanInfo classes is no longer supported", since = "2.12.0")
class BeanInfo extends scala.annotation.Annotation
