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

import scala.reflect.macros.{ReificationException, UnexpectedReificationException, TypecheckException}
import scala.reflect.macros.contexts.Context

abstract class Taggers {
  val c: Context

  import c.universe._
  import definitions._
  private val runDefinitions = currentRun.runDefinitions
  import runDefinitions._

  val coreTags = Map(
    ByteTpe -> nme.Byte,
    ShortTpe -> nme.Short,
    CharTpe -> nme.Char,
    IntTpe -> nme.Int,
    LongTpe -> nme.Long,
    FloatTpe -> nme.Float,
    DoubleTpe -> nme.Double,
    BooleanTpe -> nme.Boolean,
    UnitTpe -> nme.Unit,
    AnyTpe -> nme.Any,
    AnyValTpe -> nme.AnyVal,
    AnyRefTpe -> nme.AnyRef,
    ObjectTpe -> nme.Object,
    NothingTpe -> nme.Nothing,
    NullTpe -> nme.Null)

  def materializeClassTag(tpe: Type): Tree = {
    val tagModule = ClassTagModule
    materializeTag(EmptyTree, tpe, tagModule, {
      val erasure = c.reifyRuntimeClass(tpe, concrete = true)
      val factory = TypeApply(Select(Ident(tagModule), nme.apply), List(TypeTree(tpe)))
      Apply(factory, List(erasure))
    })
  }

  def materializeTypeTag(universe: Tree, mirror: Tree, tpe: Type, concrete: Boolean): Tree = {
    val tagType = if (concrete) TypeTagClass else WeakTypeTagClass
    // what we need here is to compose a type Universe # TypeTag[$tpe]
    // to look for an implicit that conforms to this type
    // that's why neither appliedType(tagType, List(tpe)) aka TypeRef(TypeTagsClass.thisType, tagType, List(tpe))
    // nor TypeRef(ApiUniverseClass.thisType, tagType, List(tpe)) won't fit here
    // scala> :type -v def foo: scala.reflect.api.Universe#TypeTag[Int] = ???
    // NullaryMethodType(TypeRef(pre = TypeRef(TypeSymbol(Universe)), TypeSymbol(TypeTag), args = List($tpe))))
    val unaffiliatedTagTpe = TypeRef(ApiUniverseClass.typeConstructor, tagType, List(tpe))
    val unaffiliatedTag = c.inferImplicitValue(unaffiliatedTagTpe, silent = true, withMacrosDisabled = true)
    unaffiliatedTag match {
      case success if !success.isEmpty =>
        Apply(Select(success, nme.in), List(mirror orElse mkDefaultMirrorRef(c.universe)(universe, c.callsiteTyper)))
      case _ =>
        val tagModule = if (concrete) TypeTagModule else WeakTypeTagModule
        materializeTag(universe, tpe, tagModule, c.reifyType(universe, mirror, tpe, concrete = concrete))
    }
  }

  private def materializeTag(prefix: Tree, tpe: Type, tagModule: Symbol, materializer: => Tree): Tree = {
    val result =
      tpe match {
        case coreTpe if coreTags contains coreTpe =>
          val ref = if (tagModule.isTopLevel) Ident(tagModule) else Select(prefix, tagModule.name)
          Select(ref, coreTags(coreTpe))
        case _ =>
          translatingReificationErrors(materializer)
      }
    try c.typecheck(result)
    catch { case terr: TypecheckException => failTag(result, terr) }
  }

  def materializeExpr(universe: Tree, mirror: Tree, expr: Tree): Tree = {
    val result = translatingReificationErrors(c.reifyTree(universe, mirror, expr))
    try c.typecheck(result)
    catch { case terr: TypecheckException => failExpr(result, terr) }
  }

  private def translatingReificationErrors(materializer: => Tree): Tree = {
    try materializer
    catch {
      case ReificationException(pos, msg) =>
        c.abort(pos.asInstanceOf[c.Position], msg) // this cast is a very small price for the confidence of exception handling
      case UnexpectedReificationException(_, _, cause) if cause != null =>
        throw cause
    }
  }

  private def failTag(result: Tree, reason: Any): Nothing = {
    val Apply(TypeApply(fun, List(tpeTree)), _) = c.macroApplication: @unchecked
    val tpe = tpeTree.tpe
    val PolyType(_, MethodType(_, tagTpe)) = fun.tpe: @unchecked
    val tagModule = tagTpe.typeSymbol.companionSymbol
    c.abort(c.enclosingPosition, "No %s available for %s".format(tagModule.name, tpe))
  }

  private def failExpr(result: Tree, reason: Any): Nothing = {
    val Apply(_, expr :: Nil) = c.macroApplication: @unchecked
    c.abort(c.enclosingPosition, s"Cannot materialize $expr as $result because:\n$reason")
  }
}
