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

import scala.annotation.meta.{beanGetter, beanSetter, field}

/** When attached to a field, this annotation adds a setter and a getter
 *  method following the Java Bean convention. For example:
 *  {{{
 *    @BeanProperty
 *    var status = ""
 *  }}}
 *  adds the following methods to the class:
 *  {{{
 *    def setStatus(s: String): Unit = { this.status = s }
 *    def getStatus(): String = this.status
 *  }}}
 *  For fields of type `Boolean`, if you need a getter named `isStatus`,
 *  use the `scala.beans.BooleanBeanProperty` annotation instead.
 */
@field @beanGetter @beanSetter
@deprecatedInheritance("Scheduled for being final in the future", "2.13.0")
class BeanProperty extends scala.annotation.StaticAnnotation
