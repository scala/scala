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

package scala.annotation

import scala.annotation.meta._

/** An annotation for once-ops, an operation that can be performed
  * only once on a value:
  *
  * {{{
  *   @onceop(group = "") def delete(): Unit = {}
  *   @onceop(group = "") def deleteDir(): Unit = {}
  *
  *   def bar = { delete(); deleteDir() } // show once-op warning
  * }}}
  *
  * Note that once-op invocations are grouped by the receiver
  * and the once-op group of the method, which defaults to `"default"`.
  * This means that in the above example, the call to `deleteDir()`
  * is considered to violated the once-op because both `delete`
  * and `deleteDir` methods are on the default group.
  */
@getter @setter @beanGetter @beanSetter
final class onceop(group: String = "default") extends StaticAnnotation
