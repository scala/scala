/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt
package visitor

import scala.reflect.ClassTag
import xsbti.api.{ Access, DefinitionType, Lazy, Modifiers, ParameterModifier, Variance }

/**
 * The events `ExtractAPI` emits while walking a class's API, one constructor per `xsbti.api`
 * node. Each backend chooses what a node becomes: `TreeBuilder` builds today's `xsbti.api`
 * values, `ApiHasher` builds the int sequences Zinc's `HashAPI` would fold.
 *
 * Nodes are built bottom-up and returned, rather than pushed as in ASM, so that `ExtractAPI`
 * can share them through its caches and defer structures exactly as the tree path does. The
 * small value types (`Access`, `Modifiers`, ...) are passed as they are.
 */
abstract class ApiVisitor {
  type Type <: AnyRef
  type Structure <: Type
  type PathComponent <: AnyRef
  type Annotation <: AnyRef
  type AnnotationArgument <: AnyRef
  type TypeParameter <: AnyRef
  type MethodParameter <: AnyRef
  type ParameterList <: AnyRef
  type ClassDefinition <: AnyRef
  type ClassLikeDef <: ClassDefinition
  type ClassLike <: AnyRef

  implicit def typeTag: ClassTag[Type]
  implicit def pathComponentTag: ClassTag[PathComponent]
  implicit def annotationTag: ClassTag[Annotation]
  implicit def annotationArgumentTag: ClassTag[AnnotationArgument]
  implicit def typeParameterTag: ClassTag[TypeParameter]
  implicit def methodParameterTag: ClassTag[MethodParameter]
  implicit def parameterListTag: ClassTag[ParameterList]
  implicit def classDefinitionTag: ClassTag[ClassDefinition]

  def emptyType: Type
  def parameterRef(id: String): Type
  def id(name: String): PathComponent
  def thisComponent: PathComponent
  def singleton(path: Array[PathComponent]): Type
  def projection(prefix: Type, id: String): Type
  def parameterized(base: Type, args: Array[Type]): Type
  def annotated(base: Type, annotations: Array[Annotation]): Type
  def constant(base: Type, value: String): Type
  def existential(base: Type, clause: Array[TypeParameter]): Type
  def polymorphic(base: Type, parameters: Array[TypeParameter]): Type
  def structure(
      parents: Lazy[Array[Type]],
      declared: Lazy[Array[ClassDefinition]],
      inherited: Lazy[Array[ClassDefinition]]
  ): Structure

  def annotationArgument(name: String, value: String): AnnotationArgument
  def annotation(base: Type, arguments: Array[AnnotationArgument]): Annotation
  def typeParameter(
      id: String,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      variance: Variance,
      lowerBound: Type,
      upperBound: Type
  ): TypeParameter
  def methodParameter(name: String, tpe: Type, hasDefault: Boolean, modifier: ParameterModifier): MethodParameter
  def parameterList(parameters: Array[MethodParameter], isImplicit: Boolean): ParameterList

  def defDef(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      valueParameters: Array[ParameterList],
      returnType: Type
  ): ClassDefinition
  def valDef(name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation], tpe: Type): ClassDefinition
  def varDef(name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation], tpe: Type): ClassDefinition
  def typeAlias(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      tpe: Type
  ): ClassDefinition
  def typeDeclaration(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      lowerBound: Type,
      upperBound: Type
  ): ClassDefinition
  def classLikeDef(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      definitionType: DefinitionType
  ): ClassLikeDef
  def classLike(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Annotation],
      definitionType: DefinitionType,
      selfType: Lazy[Type],
      structure: Lazy[Structure],
      savedAnnotations: Array[String],
      childrenOfSealedClass: Array[Type],
      topLevel: Boolean,
      typeParameters: Array[TypeParameter]
  ): ClassLike
}
