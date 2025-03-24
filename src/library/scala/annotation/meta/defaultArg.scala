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
package meta

/**
 * This internal meta annotation is used by the compiler to support default annotation arguments.
 *
 * For an annotation definition `class ann(x: Int = defaultExpr) extends Annotation`, the compiler adds
 * `@defaultArg(defaultExpr)` to the parameter `x`. This causes the syntax tree of `defaultExpr` to be
 * stored in the classfile.
 *
 * When using a default annotation argument, the compiler can recover the syntax tree and insert it in the
 * `AnnotationInfo`.
 *
 * For details, see `scala.reflect.internal.AnnotationInfos.AnnotationInfo`.
 */
@meta.param class defaultArg(arg: Any) extends StaticAnnotation {
  def this() = this(null)
}
