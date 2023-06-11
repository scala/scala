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

/** An annotation for an affine type, whose instance can be
  * used at-most once.
  *
  * {{{
  *   @affine class A {
  *     @affineOptOut
  *     def foo(): Unit = ()
  *   }
  *
  *   def bar = { val a = new A; a.foo(); a.foo() } // this is fine
  * }}}
  *
  */
@getter @setter @beanGetter @beanSetter
final class affineOptOut extends StaticAnnotation
