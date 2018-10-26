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

/** When attached to a field, this annotation adds a setter and a getter
 *  method following the Java Bean convention. For example:
 *  {{{
 *    @BeanProperty
 *    var status = ""
 *  }}}
 *  adds the following methods to the class:
 *  {{{
 *    def setStatus(s: String) { this.status = s }
 *    def getStatus: String = this.status
 *  }}}
 *  For fields of type `Boolean`, if you need a getter named `isStatus`,
 *  use the `scala.beans.BooleanBeanProperty` annotation instead.
 */
@scala.annotation.meta.field
class BeanProperty extends scala.annotation.StaticAnnotation
