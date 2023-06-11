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

/** An experimental annotation for an affine type, whose instance can be
  * used at-most once.
  * Currently the once-use checks are limited.
  *
  * {{{
  *   @affine class A { def foo(): Unit = () }
  *
  *   def bar = { val a = new A; a.foo(); a.foo() } // show affine warning
  * }}}
  *
  */
@getter @setter @beanGetter @beanSetter
final class affine extends StaticAnnotation
