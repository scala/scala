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
package internal
package transform

import Flags._
import scala.annotation.tailrec

trait UnCurry {

  val global: SymbolTable
  import global._
  import definitions._

  /**
   * The synthetic Java vararg method symbol corresponding to a Scala vararg method
   * annotated with @varargs.
   */
  case class VarargsSymbolAttachment(varargMethod: Symbol)

  /** Note: changing tp.normalize to tp.dealias in this method leads to a single
   *  test failure: run/t5688.scala, where instead of the expected output
   *    Vector(ta, tb, tab)
   *  we instead get
   *    Vector(tab, tb, tab)
   *  I think that difference is not the product of sentience but of randomness.
   *  The reason for this are nested RefinedTypes that are flattened by tp.normalize.
   *
   *  And normalizing AnyRef leads to another single test failure: run/t2503.scala.
   */
  private def expandAlias(tp: Type): Type =
    if (!tp.isHigherKinded && tp.typeSymbolDirect != AnyRefClass) tp.normalize else tp

  val uncurry: TypeMap = new TypeMap {
    @tailrec
    def apply(tp: Type): Type = expandAlias(tp) match {
      case MethodType(params, MethodType(params1, restpe)) =>
        // This transformation is described in UnCurryTransformer.dependentParamTypeErasure
        // Wrapping in a TypeMap to reuse the code that opts for a fast path if the function is an identity.
        val packSymbolsMap: TypeMap = packSymbols(params, _)
        val existentiallyAbstractedParam1s = packSymbolsMap.mapOver(params1)
        val substitutedResult = restpe.substSym(params1, existentiallyAbstractedParam1s)
        apply(MethodType(params ::: existentiallyAbstractedParam1s, substitutedResult))
      case MethodType(_, ExistentialType(_, MethodType(_, _))) =>
        abort("unexpected curried method types with intervening existential")
      case MethodType(h :: t, restpe) if h.isImplicit =>
        apply(MethodType(h.cloneSymbol.resetFlag(IMPLICIT) :: t, restpe))
      case NullaryMethodType(restpe) =>
        apply(MethodType(Nil, restpe))
      case DesugaredParameterType(desugaredTpe) =>
        apply(desugaredTpe)
      case other =>
        other.mapOver(this)
    }
  }

  object DesugaredParameterType {
    def unapply(tpe: Type): Option[Type] = tpe match {
      case TypeRef(_, ByNameParamClass, arg :: Nil) =>
        Some(functionType(Nil, arg))
      case TypeRef(_, RepeatedParamClass, arg :: Nil) =>
        Some(seqType(arg))
      case TypeRef(_, JavaRepeatedParamClass, arg :: Nil) =>
        Some(arrayType(if (isUnboundedGeneric(arg)) ObjectTpeJava else arg))
      case _ =>
        None
    }
  }

  private[this] val uncurryType: TypeMap = new TypeMap {
    // Not using `hasAnnotation` here because of dreaded cyclic reference errors:
    // it may happen that VarargsClass has not been initialized yet and we get here
    // while processing one of its superclasses (such as java.lang.Object). Since we
    // don't need the more precise `matches` semantics, we only check the symbol, which
    // is anyway faster and safer
    private def isVarargOverload(decl: Symbol): Boolean =
      decl.annotations.exists(_.symbol == VarargsClass) &&
        mexists(decl.paramss)(sym => definitions.isRepeatedParamType(sym.tpe))

    def apply(tp: Type): Type = expandAlias(tp) match {
      case classInfo @ ClassInfoType(parents, decls, clazz) if !clazz.isJavaDefined =>
        val newParents = parents.mapConserve(uncurry)
        val varargOverloads = for (decl <- decls if isVarargOverload(decl)) yield varargForwarderSym(clazz, decl)
        if ((newParents eq parents) && varargOverloads.isEmpty) classInfo else {
          val newDecls = decls.cloneScope
          varargOverloads.foreach(newDecls.enter)
          ClassInfoType(newParents, newDecls, clazz)
        } // @MAT normalize in decls??
      case pt: PolyType =>
        pt.mapOver(this)
      case tb: TypeBounds =>
        tb.mapOver(this)
      case other =>
        other
    }
  }

  private def varargForwarderSym(currentClass: Symbol, origSym: Symbol): Symbol = {
    val flags = VARARGS | SYNTHETIC | origSym.flags & ~DEFERRED
    val forwSym = origSym.cloneSymbol(currentClass, flags).withoutAnnotations

    // we are using `origSym.info`, which contains the type *before* the transformation
    // so we still see repeated parameter types (uncurry replaces them with Seq)
    def toArrayType(tp: Type, newParam: Symbol): Type = {
      val arg = elementType(SeqClass, tp)
      val elem = if (arg.typeSymbol.isTypeParameterOrSkolem && !(arg <:< AnyRefTpe)) {
        // To prevent generation of an `Object` parameter from `Array[T]` parameter later
        // as this would crash the Java compiler which expects an `Object[]` array for varargs
        //   e.g.        def foo[T](a: Int, b: T*)
        //   becomes     def foo[T](a: Int, b: Array[Object])
        //   instead of  def foo[T](a: Int, b: Array[T]) ===> def foo[T](a: Int, b: Object)
        //
        // In order for the forwarder method to type check we need to insert a cast:
        //   def foo'[T'](a: Int, b: Array[Object]) = foo[T'](a, wrapRefArray(b).asInstanceOf[Seq[T']])
        // The target element type for that cast (T') is stored in the  TypeParamVarargsAttachment
        //   val originalArg = arg.substSym(oldTps, tps)
        // Store the type parameter that was replaced by Object to emit the correct generic signature
        newParam.updateAttachment(TypeParamVarargsAttachment(arg))
        ObjectTpe
      } else arg
      arrayType(elem)
    }

    foreach2(forwSym.paramss, origSym.info.paramss) { (fsps, origPs) =>
      foreach2(fsps, origPs) { (p, sym) =>
        if (definitions.isRepeatedParamType(sym.tpe))
          p.setInfo(toArrayType(p.info, p))
      }
    }

    origSym.updateAttachment(VarargsSymbolAttachment(forwSym))
    forwSym
  }

  /**
   *  - return symbol's transformed type,
   *  - if symbol is a def parameter with transformed type T, return () => T
   *
   * // @MAT: starting with this phase, the info of every symbol will be normalized
   */
  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isType) uncurryType(tp)
    else if (sym.hasFlag(MODULE) && !sym.isStatic) { // see Fields::nonStaticModuleToMethod
      sym.setFlag(METHOD | STABLE)
      MethodType(Nil, uncurry(tp))
    } else uncurry(tp)
}
