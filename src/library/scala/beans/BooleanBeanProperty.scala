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

package scala.beans

import scala.annotation.meta.{beanGetter, beanSetter, field}

/** This annotation has the same functionality as
 *  `scala.beans.BeanProperty`, but the generated Bean getter will be
 *  named `isFieldName` instead of `getFieldName`.
 */
@field @beanGetter @beanSetter
@deprecatedInheritance("Scheduled for being final in the future", "2.13.0")
class BooleanBeanProperty extends scala.annotation.StaticAnnotation
