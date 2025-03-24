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

package scala.tools.nsc
package typechecker

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.Reporting.WarningCategory
import symtab.Flags._

/** Synthetic method implementations for case classes and case objects.
 *
 *  Added to all case classes/objects:
 *    def productArity: Int
 *    def productElement(n: Int): Any
 *    def productPrefix: String
 *    def productIterator: Iterator[Any]   // required for binary compatibility of value classes
 *
 *  Selectively added to case classes/objects, unless a non-default
 *  implementation already exists:
 *    def equals(other: Any): Boolean
 *    def hashCode(): Int
 *    def canEqual(other: Any): Boolean
 *    def toString(): String
 *
 *  Special handling:
 *    protected def writeReplace(): AnyRef
 */
trait SyntheticMethods extends ast.TreeDSL {
  self: Analyzer =>

  import global._
  import definitions._
  import CODE._

  private lazy val productSymbols    = List(Product_productPrefix, Product_productArity, Product_productElement) ::: Product_productElementName.toOption.toList ::: List(Product_iterator, Product_canEqual)
  private lazy val valueSymbols      = List(Any_hashCode, Any_equals)
  private lazy val caseSymbols       = List(Object_hashCode, Object_toString) ::: productSymbols
  private lazy val caseValueSymbols  = Any_toString :: valueSymbols ::: productSymbols
  private lazy val caseObjectSymbols = Object_equals :: caseSymbols
  private def symbolsToSynthesize(clazz: Symbol): List[Symbol] = {
    if (clazz.isCase) {
      if (clazz.isDerivedValueClass) caseValueSymbols
      else if (clazz.isModuleClass) caseSymbols
      else caseObjectSymbols
    }
    else if (clazz.isDerivedValueClass) valueSymbols
    else Nil
  }
  private lazy val renamedCaseAccessors = perRunCaches.newMap[Symbol, mutable.Map[TermName, TermName]]()
  /** Does not force the info of `caseclazz` */
  final def caseAccessorName(caseclazz: Symbol, paramName: TermName) =
    (renamedCaseAccessors get caseclazz).fold(paramName)(_(paramName))
  final def clearRenamedCaseAccessors(caseclazz: Symbol): Unit = {
    renamedCaseAccessors -= caseclazz
  }

  /** Add the synthetic methods to case classes.
   */
  def addSyntheticMethods(templ: Template, clazz0: Symbol, context: Context): Template = {
    val syntheticsOk = (phase.id <= currentRun.typerPhase.id) && {
      symbolsToSynthesize(clazz0).filter(_.matchingSymbol(clazz0.info).isSynthetic) match {
        case Nil  => true
        case syms => log("Not adding synthetic methods: already has " + syms.mkString(", ")) ; false
      }
    }
    if (!syntheticsOk)
      return templ

    val typer = newTyper(if (reporter.hasErrors) context.makeSilent(reportAmbiguousErrors = false) else context)
    val synthesizer = new ClassMethodSynthesis(clazz0, typer)
    import synthesizer._

    if (clazz0 == AnyValClass || isPrimitiveValueClass(clazz0)) return {
      if ((clazz0.info member nme.getClass_).isDeferred) {
        // XXX dummy implementation for now
        val getClassMethod = createMethod(nme.getClass_, getClassReturnType(clazz.tpe))(_ => NULL)
        deriveTemplate(templ)(_ :+ getClassMethod)
      }
      else templ
    }

    def accessors = clazz.caseFieldAccessors
    val arity = accessors.size

    def forwardToRuntime(method: Symbol): Tree =
      forwardMethod(method, getMember(ScalaRunTimeModule, (method.name prepend "_")))(mkThis :: _)

    def callStaticsMethodName(name: TermName)(args: Tree*): Tree = {
      val method = RuntimeStaticsModule.info.member(name)
      Apply(gen.mkAttributedRef(method), args.toList)
    }

    def callStaticsMethod(name: String)(args: Tree*): Tree =
      callStaticsMethodName(newTermName(name))(args: _*)

    // Any concrete member, including private
    def hasConcreteImpl(name: Name) =
      clazz.info.member(name).alternatives exists (m => !m.isDeferred)

    def hasOverridingImplementation(meth: Symbol) = {
      val sym = clazz.info nonPrivateMember meth.name
      sym.alternatives exists { m0 =>
        (m0 ne meth) && !m0.isDeferred && !m0.isSynthetic && (m0.owner != AnyValClass) && (typeInClazz(m0) matches typeInClazz(meth))
      }
    }
    def productIteratorMethod =
      createMethod(nme.productIterator, iteratorOfType(AnyTpe))(_ =>
        gen.mkMethodCall(ScalaRunTimeModule, nme.typedProductIterator, List(AnyTpe), List(mkThis))
      )

    def perElementMethod(name: Name, returnType: Type)(caseFn: Symbol => Tree): Tree = 
      createSwitchMethod(name, accessors.indices, returnType)(idx => caseFn(accessors(idx)))

    def productElementNameMethod = {
      val elementAccessors = clazz.constrParamAccessors.take(arity)
      createSwitchMethod(nme.productElementName, elementAccessors.indices, StringTpe)(idx => LIT(elementAccessors(idx).name.dropLocal.decode))
    }

    var syntheticCanEqual = false

    /* The canEqual method for case classes.
     *   def canEqual(that: Any) = that.isInstanceOf[This]
     */
    def canEqualMethod: Tree = {
      syntheticCanEqual = true
      createMethod(nme.canEqual_, List(AnyTpe), BooleanTpe) { m =>
        Ident(m.firstParam) IS_OBJ classExistentialType(context.prefix, clazz)
      }
    }

    /* that match { case _: this.C => true ; case _ => false }
     * where `that` is the given method's first parameter.
     *
     * An isInstanceOf test is insufficient because it has weaker
     * requirements than a pattern match. Given an inner class Foo and
     * two different instantiations of the container, an x.Foo and and a y.Foo
     * are both .isInstanceOf[Foo], but the one does not match as the other.
     */
    def thatTest(eqmeth: Symbol): Tree = {
      Match(
        Ident(eqmeth.firstParam),
        List(
          CaseDef(Typed(Ident(nme.WILDCARD), TypeTree(typer.applyTypeToWildcards(clazz.tpe))), EmptyTree, TRUE),
          CaseDef(Ident(nme.WILDCARD), EmptyTree, FALSE)
        )
      )
    }

    /* (that.asInstanceOf[this.C])
     * where that is the given methods first parameter.
     */
    def thatCast(eqmeth: Symbol): Tree =
      gen.mkCast(Ident(eqmeth.firstParam), clazz.tpe)

    /* The equality method core for case classes and derived value classes.
     * Generally:
     * 1+ args:
     *   (that.isInstanceOf[this.C]) && {
     *       val x$1 = that.asInstanceOf[this.C]
     *       (this.arg_1 == x$1.arg_1) && (this.arg_2 == x$1.arg_2) && ... && (x$1 canEqual this)
     *      }
     * Drop:
     * - canEqual part if class is final and canEqual is synthesized.
     * - test for arg_i if arg_i has type Nothing, Null, or Unit
     * - asInstanceOf if no equality checks need made (see scala/bug#9240, scala/bug#10361)
     */
    def equalsCore(eqmeth: Symbol, accessors: List[Symbol]) = {
      val otherName = freshTermName(clazz.name.toStringWithSuffix("$"))(freshNameCreatorFor(context))
      val otherSym  = eqmeth.newValue(otherName, eqmeth.pos, SYNTHETIC) setInfo clazz.tpe
      val pairwise  = {
        //compare primitive fields first, slow equality checks of non-primitive fields can be skipped when primitives differ
        val prims = ListBuffer[Tree]()
        val refs  = ListBuffer[Tree]()
        for (acc <- accessors) {
          val resultType   = acc.info.resultType
          val usefulEquals = resultType != NothingTpe && resultType != NullTpe && resultType != UnitTpe
          if (usefulEquals) {
            val thisAcc  = Select(mkThis, acc)
            val otherAcc = Select(Ident(otherSym), acc)
            if (isPrimitiveValueType(resultType))
              prims += fn(thisAcc, acc.tpe.member(nme.EQ), otherAcc)
            else
              refs  += fn(thisAcc, Any_==, otherAcc)
          }
        }
        prims.prependToList(refs.toList)      // (prims ++ refs).toList
      }
      val canEq     = gen.mkMethodCall(otherSym, nme.canEqual_, Nil, List(mkThis))
      val tests     = if (clazz.isDerivedValueClass || clazz.isFinal && syntheticCanEqual) pairwise else pairwise :+ canEq

      if (tests.isEmpty) {
        thatTest(eqmeth)
      } else {
        thatTest(eqmeth) AND Block(
          ValDef(otherSym, thatCast(eqmeth)),
          AND(tests: _*)
        )
      }
    }

    /* The equality method for case classes.
     * 0 args:
     *   def equals(that: Any) = that.isInstanceOf[this.C] && that.asInstanceOf[this.C].canEqual(this)
     * 1+ args:
     *   def equals(that: Any) = (this eq that.asInstanceOf[AnyRef]) || {
     *     (that.isInstanceOf[this.C]) && {
     *       val x$1 = that.asInstanceOf[this.C]
     *       (this.arg_1 == x$1.arg_1) && (this.arg_2 == x$1.arg_2) && ... && (x$1 canEqual this)
     *      }
     *   }
     */
    def equalsCaseClassMethod: Tree = createMethod(nme.equals_, List(AnyTpe), BooleanTpe) { m =>
      if (accessors.isEmpty)
        if (clazz.isFinal) thatTest(m)
        else thatTest(m) AND ((thatCast(m) DOT nme.canEqual_)(mkThis))
      else
        (mkThis ANY_EQ Ident(m.firstParam)) OR equalsCore(m, accessors)
    }

    /* The equality method for value classes
     * def equals(that: Any) = (this.asInstanceOf[AnyRef]) eq that.asInstanceOf[AnyRef]) || {
     *   (that.isInstanceOf[this.C]) && {
     *    val x$1 = that.asInstanceOf[this.C]
     *    (this.underlying == that.underlying
     */
    def equalsDerivedValueClassMethod: Tree = createMethod(nme.equals_, List(AnyTpe), BooleanTpe) { m =>
      equalsCore(m, List(clazz.derivedValueClassUnbox))
    }

    /* The hashcode method for value classes
     * def hashCode(): Int = this.underlying.hashCode
     */
    def hashCodeDerivedValueClassMethod: Tree = createMethod(nme.hashCode_, Nil, IntTpe) { _ =>
      Select(mkThisSelect(clazz.derivedValueClassUnbox), nme.hashCode_)
    }

    /* The _1, _2, etc. methods to implement ProductN, disabled
     * until we figure out how to introduce ProductN without cycles.
     */
    /*
    def productNMethods = {
      val accs = accessors.toIndexedSeq
      1 to arity map (num => productProj(arity, num) -> (() => projectionMethod(accs(num - 1), num)))
    }
    def projectionMethod(accessor: Symbol, num: Int) = {
      createMethod(nme.productAccessorName(num), accessor.tpe.resultType)(_ => REF(accessor))
    }
    */

    // methods for both classes and objects
    def productMethods: List[(Symbol, () => Tree)] = {
      List(
        Product_productPrefix -> (() => constantNullary(nme.productPrefix, clazz.name.decode)),
        Product_productArity -> (() => constantNullary(nme.productArity, arity)),
        Product_productElement -> (() => perElementMethod(nme.productElement, AnyTpe)(mkThisSelect)),
        Product_iterator -> (() => productIteratorMethod),
        Product_canEqual -> (() => canEqualMethod)
      )
    }

    def productClassMethods: List[(Symbol, () => Tree)] = {
      // Classes get productElementName but case objects do not.
      // For a case object the correct behaviour (i.e. to throw an IOOBE)
      // is already provided by the default implementation in the Product trait.

      // Support running the compiler with an older library on the classpath
      def elementName: List[(Symbol, () => Tree)] = Product_productElementName match {
        case NoSymbol => Nil
        case sym => (sym, () => productElementNameMethod) :: Nil
      }

      productMethods ::: elementName
    }

    def hashcodeImplementation(sym: Symbol): Tree = {
      sym.tpe.finalResultType.typeSymbol match {
        case UnitClass | NullClass              => Literal(Constant(0))
        case BooleanClass                       => If(Ident(sym), Literal(Constant(1231)), Literal(Constant(1237)))
        case IntClass                           => Ident(sym)
        case ShortClass | ByteClass | CharClass => Select(Ident(sym), nme.toInt)
        case LongClass                          => callStaticsMethodName(nme.longHash)(Ident(sym))
        case DoubleClass                        => callStaticsMethodName(nme.doubleHash)(Ident(sym))
        case FloatClass                         => callStaticsMethodName(nme.floatHash)(Ident(sym))
        case _                                  => callStaticsMethodName(nme.anyHash)(Ident(sym))
      }
    }

    def specializedHashcode = {
      createMethod(nme.hashCode_, Nil, IntTpe) { m =>
        val accumulator = m.newVariable(newTermName("acc"), m.pos, SYNTHETIC) setInfo IntTpe
        val valdef      = ValDef(accumulator, Literal(Constant(0xcafebabe)))
        val mixPrefix   =
          Assign(
            Ident(accumulator),
            callStaticsMethod("mix")(Ident(accumulator),
              Apply(gen.mkAttributedSelect(gen.mkAttributedSelect(mkThis, Product_productPrefix), Object_hashCode), Nil)))
        val mixes       = accessors map (acc =>
          Assign(
            Ident(accumulator),
            callStaticsMethod("mix")(Ident(accumulator), hashcodeImplementation(acc))
          )
        )
        val finish = callStaticsMethod("finalizeHash")(Ident(accumulator), Literal(Constant(arity)))

        Block(valdef :: mixPrefix :: mixes, finish)
      }
    }
    def chooseHashcode = {
      if (accessors exists (x => isPrimitiveValueType(x.tpe.finalResultType)))
        specializedHashcode
      else
        forwardToRuntime(Object_hashCode)
    }

    def valueClassMethods = List(
      Any_hashCode -> (() => hashCodeDerivedValueClassMethod),
      Any_equals -> (() => equalsDerivedValueClassMethod)
    )

    def caseClassMethods = productClassMethods ++ /*productNMethods ++*/ Seq(
      Object_hashCode -> (() => chooseHashcode),
      Object_toString -> (() => forwardToRuntime(Object_toString)),
      Object_equals   -> (() => equalsCaseClassMethod)
    )

    def valueCaseClassMethods = productClassMethods ++ /*productNMethods ++*/ valueClassMethods ++ Seq(
      Any_toString -> (() => forwardToRuntime(Object_toString))
    )

    def caseObjectMethods = productMethods ++ Seq(
      Object_hashCode -> (() => constantMethod(nme.hashCode_, clazz.name.decode.hashCode)),
      Object_toString -> (() => constantMethod(nme.toString_, clazz.name.decode))
      // Not needed, as reference equality is the default.
      // Object_equals   -> (() => createMethod(Object_equals)(m => This(clazz) ANY_EQ Ident(m.firstParam)))
    )

    /* If you serialize a singleton you will get an additional 
     * instance of the singleton, unless you implement 
     * special serialization logic.  Here we use a serialization proxy that prevents 
     * serialization of state and will, on deserialization by replaced by the object
     * via use of readResolve. This is done for all top level objects which extend
     * `java.io.Serializable` (such as case objects)
     */
    def needsModuleSerializationProxy = (
         clazz.isModuleClass
      && clazz.isSerializable
      && !hasConcreteImpl(nme.writeReplace)
      && clazz.isStatic
    )

    def synthesize(): List[Tree] = {
      val methods = (
        if (clazz.isCase)
          if (clazz.isDerivedValueClass) valueCaseClassMethods
          else if (clazz.isModuleClass) caseObjectMethods
          else caseClassMethods
        else if (clazz.isDerivedValueClass) valueClassMethods
        else Nil
      )

      /* Always generate overrides for equals and hashCode in value classes,
       * so they can appear in universal traits without breaking value semantics.
       */
      def impls = {
        def shouldGenerate(m: Symbol) = {
          !hasOverridingImplementation(m) || {
            clazz.isDerivedValueClass && (m == Any_hashCode || m == Any_equals) && {
              // Without a means to suppress this warning, I've thought better of it.
              if (settings.warnValueOverrides.value) {
                 (clazz.info nonPrivateMember m.name) filter (m => (m.owner != AnyClass) && (m.owner != clazz) && !m.isDeferred) andAlso { m =>
                   typer.context.warning(clazz.pos, s"Implementation of ${m.name} inherited from ${m.owner} overridden in $clazz to enforce value class semantics", WarningCategory.Other /* settings.warnValueOverrides is not exposed as compiler flag */)
                 }
               }
              true
            }
          }
        }
        for ((m, impl) <- methods ; if shouldGenerate(m)) yield impl()
      }
      def extras = {
        if (needsModuleSerializationProxy) {
          // Aha, I finally decoded the original comment.
          // This method should be generated as private, but apparently if it is, then
          // it is name mangled afterward.  (Wonder why that is.) So it's only protected.
          // For sure special methods like "writeReplace" should not be mangled.
          List(createMethod(nme.writeReplace, Nil, ObjectTpe)(m => {
            m setFlag PRIVATE
            New(ModuleSerializationProxyClass, gen.mkClassOf(clazz.typeOfThis))
          }))
        }
        else Nil
      }

      try impls ++ extras
      catch { case _: TypeError if reporter.hasErrors => Nil }
    }

    /* If this case class has any less than public accessors,
     * adds new accessors at the correct locations to preserve ordering.
     * Note that this must be done before the other method synthesis
     * because synthesized methods need refer to the new symbols.
     * Care must also be taken to preserve the case accessor order.
     */
    def caseTemplateBody(): List[Tree] = {
      val lb = ListBuffer[Tree]()
      def isRewrite(sym: Symbol) = sym.isCaseAccessorMethod && !sym.isPublic

      for (ddef @ DefDef(_, _, _, _, _, _) <- templ.body ; if isRewrite(ddef.symbol)) {
        val original = ddef.symbol
        val i = original.owner.caseFieldAccessors.indexOf(original)
        def freshAccessorName = {
          devWarning(s"Unable to find $original among case accessors of ${original.owner}: ${original.owner.caseFieldAccessors}")
          freshTermName(original.name.toStringWithSuffix("$"))(freshNameCreatorFor(context))
        }
        def nameSuffixedByParamIndex = original.name.append(s"${nme.CASE_ACCESSOR}$$${i}").toTermName
        val newName = if (i < 0) freshAccessorName else nameSuffixedByParamIndex
        val newAcc = deriveMethod(ddef.symbol, _ => newName) { newAcc =>
          newAcc.makePublic
          newAcc resetFlag (ACCESSOR | PARAMACCESSOR | OVERRIDE)
          ddef.rhs.duplicate
        }
        // TODO: shouldn't the next line be: `original resetFlag CASEACCESSOR`?
        ddef.symbol resetFlag CASEACCESSOR
        lb += logResult("case accessor new")(newAcc)
        val renamedInClassMap = renamedCaseAccessors.getOrElseUpdate(clazz, mutable.Map() withDefault(x => x))
        renamedInClassMap(original.name.toTermName) = newAcc.symbol.name.toTermName
      }

      (lb ++= templ.body ++= synthesize()).toList
    }

    deriveTemplate(templ)(body =>
      if (clazz.isCase) caseTemplateBody()
      else synthesize() match {
        case Nil  => body // avoiding unnecessary copy
        case ms   => body ++ ms
      }
    )
  }
}
