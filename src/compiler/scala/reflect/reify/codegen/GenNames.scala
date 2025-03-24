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

package scala.reflect.reify
package codegen

trait GenNames {
  self: Reifier =>

  import global._

  def reifyName(name: Name) = {
    val factory = if (name.isTypeName) nme.TypeName else nme.TermName
    mirrorCall(factory, Literal(Constant(name.toString)))
  }
}
