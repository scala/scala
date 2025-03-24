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

package scala.reflect
package api

trait Quasiquotes { self: Universe =>

  /** Implicit class that introduces `q`, `tq`, `cq,` `pq` and `fq` string interpolators
   *  that are also known as quasiquotes. With their help you can easily manipulate
   *  Scala reflection ASTs.
   *
   *  @see [[https://docs.scala-lang.org/overviews/quasiquotes/intro.html]]
   */
  implicit class Quasiquote(ctx: StringContext) {
    protected trait api {
      // implementation is hardwired to `dispatch` method of `scala.tools.reflect.quasiquotes.Quasiquotes`
      // using the mechanism implemented in `scala.tools.reflect.FastTrack`
      def apply[A >: Any](args: A*): Tree = macro ???
      def unapply(scrutinee: Any): Any = macro ???
    }
    object q extends api
    object tq extends api
    object cq extends api
    object pq extends api
    object fq extends api
  }
}
