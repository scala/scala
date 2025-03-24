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

package scala

import Specializable._

/** Annotate type parameters on which code should be automatically
 *  specialized. For example:
 *  {{{
 *    class MyList[@specialized T] ...
 *  }}}
 *
 *  Type T can be specialized on a subset of the primitive types by
 *  specifying a list of primitive types to specialize at:
 *  {{{
 *    class MyList[@specialized(Int, Double, Boolean) T] ..
 *  }}}
 */
// class tspecialized[T](group: Group[T]) extends scala.annotation.StaticAnnotation {

final class specialized(group: SpecializedGroup) extends scala.annotation.StaticAnnotation {
  def this(types: Specializable*) = this(new Group(types.toList))
  def this() = this(Primitives)
}
