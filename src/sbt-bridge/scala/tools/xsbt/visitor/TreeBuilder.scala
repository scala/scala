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
import xsbti.api
import xsbti.api.{ Access, DefinitionType, Lazy, Modifiers, ParameterModifier, Variance }

/** Builds the `xsbti.api` trees that Zinc stores and hashes. */
final class TreeBuilder extends ApiVisitor {
  type Type = api.Type
  type Structure = api.Structure
  type PathComponent = api.PathComponent
  type Annotation = api.Annotation
  type AnnotationArgument = api.AnnotationArgument
  type TypeParameter = api.TypeParameter
  type MethodParameter = api.MethodParameter
  type ParameterList = api.ParameterList
  type ClassDefinition = api.ClassDefinition
  type ClassLikeDef = api.ClassLikeDef
  type ClassLike = api.ClassLike

  implicit val typeTag: ClassTag[Type] = ClassTag(classOf[api.Type])
  implicit val pathComponentTag: ClassTag[PathComponent] = ClassTag(classOf[api.PathComponent])
  implicit val annotationTag: ClassTag[Annotation] = ClassTag(classOf[api.Annotation])
  implicit val annotationArgumentTag: ClassTag[AnnotationArgument] = ClassTag(classOf[api.AnnotationArgument])
  implicit val typeParameterTag: ClassTag[TypeParameter] = ClassTag(classOf[api.TypeParameter])
  implicit val methodParameterTag: ClassTag[MethodParameter] = ClassTag(classOf[api.MethodParameter])
  implicit val parameterListTag: ClassTag[ParameterList] = ClassTag(classOf[api.ParameterList])
  implicit val classDefinitionTag: ClassTag[ClassDefinition] = ClassTag(classOf[api.ClassDefinition])

  private[this] val theEmptyType = api.EmptyType.of()
  private[this] val theThis = api.This.of()

  def emptyType: Type = theEmptyType
  def parameterRef(id: String): Type = api.ParameterRef.of(id)
  def id(name: String): PathComponent = api.Id.of(name)
  def thisComponent: PathComponent = theThis
  def singleton(path: Array[PathComponent]): Type = api.Singleton.of(api.Path.of(path))
  def projection(prefix: Type, id: String): Type = api.Projection.of(prefix, id)
  def parameterized(base: Type, args: Array[Type]): Type = api.Parameterized.of(base, args)
  def annotated(base: Type, annotations: Array[Annotation]): Type = api.Annotated.of(base, annotations)
  def constant(base: Type, value: String): Type = api.Constant.of(base, value)
  def existential(base: Type, clause: Array[TypeParameter]): Type = api.Existential.of(base, clause)
  def polymorphic(base: Type, parameters: Array[TypeParameter]): Type = api.Polymorphic.of(base, parameters)
  def structure(
      parents: Lazy[Array[Type]],
      declared: Lazy[Array[ClassDefinition]],
      inherited: Lazy[Array[ClassDefinition]]
  ): Structure = api.Structure.of(parents, declared, inherited)

  def annotationArgument(name: String, value: String): AnnotationArgument = api.AnnotationArgument.of(name, value)
  def annotation(base: Type, arguments: Array[AnnotationArgument]): Annotation = api.Annotation.of(base, arguments)
  def typeParameter(
      id: String,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      variance: Variance,
      lowerBound: Type,
      upperBound: Type
  ): TypeParameter = api.TypeParameter.of(id, annotations, typeParameters, variance, lowerBound, upperBound)
  def methodParameter(name: String, tpe: Type, hasDefault: Boolean, modifier: ParameterModifier): MethodParameter =
    api.MethodParameter.of(name, tpe, hasDefault, modifier)
  def parameterList(parameters: Array[MethodParameter], isImplicit: Boolean): ParameterList =
    api.ParameterList.of(parameters, isImplicit)

  def defDef(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      valueParameters: Array[ParameterList],
      returnType: Type
  ): ClassDefinition = api.Def.of(name, access, modifiers, annotations, typeParameters, valueParameters, returnType)
  def valDef(name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation], tpe: Type): ClassDefinition =
    api.Val.of(name, access, modifiers, annotations, tpe)
  def varDef(name: String, access: Access, modifiers: Modifiers, annotations: Array[Annotation], tpe: Type): ClassDefinition =
    api.Var.of(name, access, modifiers, annotations, tpe)
  def typeAlias(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      tpe: Type
  ): ClassDefinition = api.TypeAlias.of(name, access, modifiers, annotations, typeParameters, tpe)
  def typeDeclaration(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      lowerBound: Type,
      upperBound: Type
  ): ClassDefinition =
    api.TypeDeclaration.of(name, access, modifiers, annotations, typeParameters, lowerBound, upperBound)
  def classLikeDef(
      name: String,
      access: Access,
      modifiers: Modifiers,
      annotations: Array[Annotation],
      typeParameters: Array[TypeParameter],
      definitionType: DefinitionType
  ): ClassLikeDef = api.ClassLikeDef.of(name, access, modifiers, annotations, typeParameters, definitionType)
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
  ): ClassLike =
    api.ClassLike.of(
      name,
      access,
      modifiers,
      annotations,
      definitionType,
      selfType,
      structure,
      savedAnnotations,
      childrenOfSealedClass,
      topLevel,
      typeParameters
    )
}
