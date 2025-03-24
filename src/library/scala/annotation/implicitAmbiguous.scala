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

package scala.annotation

/**
  * To customize the error message that's emitted when an implicit search finds
  * multiple ambiguous values, annotate at least one of the implicit values
  * `@implicitAmbiguous`. Assuming the implicit value is a method with type
  * parameters `X1,..., XN`, the error message will be the result of replacing
  * all occurrences of `\${Xi}` in the string `msg` with the string representation
  * of the corresponding type argument `Ti`.
  *
  * If more than one `@implicitAmbiguous` annotation is collected, the compiler is
  * free to pick any of them to display.
  *
  * Nice errors can direct users to fix imports or even tell them why code
  * intentionally doesn't compile.
  *
  * {{{
  * trait =!=[C, D]
  *
  * implicit def neq[E, F] : E =!= F = null
  *
  * @annotation.implicitAmbiguous("Could not prove \${J} =!= \${J}")
  * implicit def neqAmbig1[G, H, J] : J =!= J = null
  * implicit def neqAmbig2[I] : I =!= I = null
  *
  * implicitly[Int =!= Int]
  * }}}
  */
@meta.getter
final class implicitAmbiguous(msg: String) extends scala.annotation.ConstantAnnotation
