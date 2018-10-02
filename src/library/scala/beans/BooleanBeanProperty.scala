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

/** This annotation has the same functionality as
 *  `scala.beans.BeanProperty`, but the generated Bean getter will be
 *  named `isFieldName` instead of `getFieldName`.
 */
@scala.annotation.meta.field
class BooleanBeanProperty extends scala.annotation.StaticAnnotation
