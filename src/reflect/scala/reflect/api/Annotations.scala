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

package scala
package reflect
package api

import scala.collection.immutable.ListMap

/**
 *  <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  This trait provides annotation support for the reflection API.
 *
 *  In Scala, annotations belong to one of the two categories:
 *
 *  <ul>
 *  <li>''Java annotations'': annotations on definitions produced by the Java compiler, i.e., subtypes of [[java.lang.annotation.Annotation]]
 *  attached to program definitions.</li>
 *  <li>''Scala annotations'': annotations on definitions or types produced by the Scala compiler.</li>
 *  </ul>
 *
 *  When a Scala annotation that inherits from [[scala.annotation.StaticAnnotation]] is compiled,
 *  it is stored as special attributes in the corresponding classfile, and not as a Java annotation. Note that subclassing
 *  just [[scala.annotation.Annotation]] is not enough to have the corresponding metadata persisted for runtime reflection.
 *
 *  Both Java and Scala annotations are represented as typed trees carrying constructor invocations corresponding
 *  to the annotation. For instance, the annotation in `@ann(1, 2) class C` is represented as `q"@new ann(1, 2)"`.
 *
 *  Unlike Java reflection, Scala reflection does not support evaluation of constructor invocations stored in annotations
 *  into underlying objects. For instance it's impossible to go from `@ann(1, 2) class C` to `ann(1, 2)`, so one
 *  has to analyze trees representing annotation arguments to manually extract corresponding values. Towards that end,
 *  arguments of an annotation can be obtained via `annotation.tree.children.tail`.
 *
 *  For more information about `Annotation`s, see the [[https://docs.scala-lang.org/overviews/reflection/annotations-names-scopes.html Reflection Guide: Annotations, Names, Scopes, and More]]
 *
 *  @contentDiagram hideNodes "*Api"
 *  @group ReflectionAPI
 */
trait Annotations { self: Universe =>

  /** Information about an annotation.
   *  @template
   *  @group Annotations
   */
  type Annotation >: Null <: AnyRef with AnnotationApi

  /** The constructor/extractor for `Annotation` instances.
   *  @group Extractors
   */
  val Annotation: AnnotationExtractor

  /** An extractor class to create and pattern match with syntax `Annotation(tpe, scalaArgs, javaArgs)`.
   *  Here, `tpe` is the annotation type, `scalaArgs` the payload of Scala annotations, and `javaArgs` the payload of Java annotations.
    *  @group Extractors
    */
  abstract class AnnotationExtractor {
    def apply(tree: Tree): Annotation = treeToAnnotation(tree)

    @deprecated("use `apply(tree: Tree): Annotation` instead", "2.11.0")
    def apply(tpe: Type, scalaArgs: List[Tree], javaArgs: ListMap[Name, JavaArgument]): Annotation

    @deprecated("use `Annotation.tree` to inspect annotation arguments", "2.11.0")
    def unapply(ann: Annotation): Option[(Type, List[Tree], ListMap[Name, JavaArgument])]
  }

  /** The API of `Annotation` instances.
   *  The main source of information about annotations is the [[scala.reflect.api.Annotations]] page.
   *  @group API
   */
  trait AnnotationApi {
    /** The tree underlying the annotation. */
    def tree: Tree = annotationToTree(this.asInstanceOf[Annotation])

    /** The type of the annotation. */
    @deprecated("use `tree.tpe` instead", "2.11.0")
    def tpe: Type

    /** Payload of the Scala annotation: a list of abstract syntax trees that represent the argument.
     *  Empty for Java annotations.
     */
    @deprecated("use `tree.children.tail` instead", "2.11.0")
    def scalaArgs: List[Tree]

    /** Payload of the Java annotation: a list of name-value pairs.
     *  Empty for Scala annotations.
     */
    @deprecated("use `tree.children.tail` instead", "2.11.0")
    def javaArgs: ListMap[Name, JavaArgument]
  }

  protected[scala] def annotationToTree(ann: Annotation): Tree
  protected[scala] def treeToAnnotation(tree: Tree): Annotation

  /** A Java annotation argument
   *  @template
   *  @group Annotations
   */
  @deprecated("use `Annotation.tree` to inspect annotation arguments", "2.11.0")
  type JavaArgument >: Null <: AnyRef with JavaArgumentApi

  /** Has no special methods. Is here to provides erased identity for `CompoundType`.
   *  @group API
   */
  @deprecated("use `Annotation.tree` to inspect annotation arguments", "2.11.0")
  trait JavaArgumentApi
}
