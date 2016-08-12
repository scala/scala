/* NSC -- new Scala compiler -- Copyright 2007-2013 LAMP/EPFL
 *
 * This trait finds implicit conversions for a class in the default scope and creates scaladoc entries for each of them.
 *
 * @author Vlad Ureche
 * @author Adriaan Moors
 */

package scala.tools.nsc
package doc
package model

import scala.collection._

/**
 * This trait finds implicit conversions for a class in the default scope and creates scaladoc entries for each of them.
 *
 * Let's take this as an example:
 * {{{
 *    object Test {
 *      class A
 *
 *      class B {
 *        def foo = 1
 *      }
 *
 *      class C extends B {
 *        def bar = 2
 *        class implicit
 *      }
 *
 *      D def conv(a: A) = new C
 *    }
 * }}}
 *
 * Overview:
 * - scaladoc-ing the above classes, `A` will get two more methods: foo and bar, over its default methods
 * - the nested classes (specifically `D` above), abstract types, type aliases and constructor members are not added to
 * `A` (see makeMember0 in ModelFactory, last 3 cases)
 * - the members added by implicit conversion are always listed under the implicit conversion, not under the class they
 * actually come from (`foo` will be listed as coming from the implicit conversion to `C` instead of `B`) - see
 * `definitionName` in MemberImpl
 *
 * Internals:
 * TODO: Give an overview here
 */
trait ModelFactoryImplicitSupport {
  thisFactory: ModelFactory with ModelFactoryTypeSupport with CommentFactory with TreeFactory =>

  import global._
  import global.analyzer._
  import global.definitions._
  import settings.hardcoded

  // debugging:
  val DEBUG: Boolean = settings.docImplicitsDebug.value
  val ERROR: Boolean = true // currently we show all errors
  @inline final def debug(msg: => String) = if (DEBUG) settings.printMsg(msg)
  @inline final def error(msg: => String) = if (ERROR) settings.printMsg(msg)

  /** This is a flag that indicates whether to eliminate implicits that cannot be satisfied within the current scope.
   * For example, if an implicit conversion requires that there is a Numeric[T] in scope:
   *  {{{
   *     class A[T]
   *     class B extends A[Int]
   *     class C extends A[String]
   *     implicit def enrichA[T: Numeric](a: A[T]): D
   *  }}}
   *  For B, no constraints are generated as Numeric[Int] is already in the default scope. On the other hand, for the
   *  conversion from C to D, depending on -implicits-show-all, the conversion can:
   *   - not be generated at all, since there's no Numeric[String] in scope (if ran without -implicits-show-all)
   *   - generated with a *weird* constraint, Numeric[String] as the user might add it by hand (if flag is enabled)
   */
  class ImplicitNotFound(tpe: Type) extends Exception("No implicit of type " + tpe + " found in scope.")

  /* ============== MAKER METHODS ============== */

  /**
   *  Make the implicit conversion objects
   *
   *  A word about the scope of the implicit conversions: currently we look at a very basic context composed of the
   *  default Scala imports (Predef._ for example) and the companion object of the current class, if one exists. In the
   *  future we might want to extend this to more complex scopes.
   */
  def makeImplicitConversions(sym: Symbol, inTpl: DocTemplateImpl): List[ImplicitConversionImpl] =
    // Nothing and Null are somewhat special -- they can be transformed by any implicit conversion available in scope.
    // But we don't want that, so we'll simply refuse to find implicit conversions on for Nothing and Null
    if (!(sym.isClass || sym.isTrait || sym == AnyRefClass) || sym == NothingClass || sym == NullClass) Nil
    else {
      val context: global.analyzer.Context = global.analyzer.rootContext(NoCompilationUnit)

      val results = global.analyzer.allViewsFrom(sym.tpe_*, context, sym.typeParams) ++
        global.analyzer.allViewsFrom(byNameType(sym.tpe_*), context, sym.typeParams)
      var conversions = results.flatMap(result => makeImplicitConversion(sym, result._1, result._2, context, inTpl))
      //debug(results.mkString("All views\n  ", "\n  ", "\n"))
      //debug(conversions.mkString("Conversions\n  ", "\n  ", "\n"))

      // also keep empty conversions, so they appear in diagrams
      // conversions = conversions.filter(!_.members.isEmpty)

      val hiddenConversions: Seq[String] = thisFactory
        .comment(sym, inTpl.linkTarget, inTpl)
        .map(_.hideImplicitConversions)
        .getOrElse(Nil)

      conversions = conversions filterNot { conv: ImplicitConversionImpl =>
        hiddenConversions.contains(conv.conversionShortName) ||
        hiddenConversions.contains(conv.conversionQualifiedName)
      }

      // Filter out non-sensical conversions from value types
      if (isPrimitiveValueType(sym.tpe_*))
        conversions = conversions.filter((ic: ImplicitConversionImpl) =>
          hardcoded.valueClassFilter(sym.nameString, ic.conversionQualifiedName))

      // Put the visible conversions in front
      val (ownConversions, commonConversions) =
        conversions.partition(!_.isHiddenConversion)

      ownConversions ::: commonConversions
    }

  /** makeImplicitConversion performs the heavier lifting to get the implicit listing:
   * - for each possible conversion function (also called view)
   *    * figures out the final result of the view (to what is our class transformed?)
   *    * figures out the necessary constraints on the type parameters (such as T <: Int) and the context (such as Numeric[T])
   *    * lists all inherited members
   *
   * What? in details:
   *  - say we start from a class A[T1, T2, T3, T4]
   *  - we have an implicit function (view) in scope:
   *     def enrichA[T3 <: Long, T4](a: A[Int, Foo[Bar[X]], T3, T4])(implicit ev1: TypeTag[T4], ev2: Numeric[T4]): EnrichedA
   *  - A is converted to EnrichedA ONLY if a couple of constraints are satisfied:
   *     * T1 must be equal to Int
   *     * T2 must be equal to Foo[Bar[X]]
   *     * T3 must be upper bounded by Long
   *     * there must be evidence of Numeric[T4] and a TypeTag[T4] within scope
   *  - the final type is EnrichedA and A therefore inherits a couple of members from enrichA
   *
   * How?
   * some notes:
   *  - Scala's type inference will want to solve all type parameters down to actual types, but we only want constraints
   * to maintain generality
   *  - therefore, allViewsFrom wraps type parameters into "untouchable" type variables that only gather constraints,
   * but are never solved down to a type
   *  - these must be reverted back to the type parameters and the constraints must be extracted and simplified (this is
   * done by the uniteConstraints and boundedTParamsConstraints. Be sure to check them out
   *  - we also need to transform implicit parameters in the view's signature into constraints, such that Numeric[T4]
   * appears as a constraint
   */
  def makeImplicitConversion(sym: Symbol, result: SearchResult, constrs: List[TypeConstraint], context: Context, inTpl: DocTemplateImpl): List[ImplicitConversionImpl] =
    if (result.tree == EmptyTree) Nil
    else {
      // `result` will contain the type of the view (= implicit conversion method)
      // the search introduces untouchable type variables, but we want to get back to type parameters
      val viewFullType = result.tree.tpe
      // set the previously implicit parameters to being explicit

      val (viewSimplifiedType, viewImplicitTypes) = removeImplicitParameters(viewFullType)

      // TODO: Isolate this corner case :) - Predef.<%< and put it in the testsuite
      if (viewSimplifiedType.params.length != 1) {
        // This is known to be caused by the `<%<` object in Predef:
        // {{{
        //    sealed abstract class <%<[-From, +To] extends (From => To) with Serializable
        //    object <%< {
        //      implicit def conformsOrViewsAs[A <% B, B]: A <%< B = new (A <%< B) {def apply(x: A) = x}
        //    }
        // }}}
        // so we just won't generate an implicit conversion for implicit methods that only take implicit parameters
        return Nil
      }

      if (!settings.docImplicitsShowAll && viewSimplifiedType.resultType.typeSymbol == sym) {
        // If, when looking at views for a class A, we find one that returns A as well
        // (possibly with different type parameters), we ignore it.
        // It usually is a way to build a "whatever" into an A, but we already have an A, as in:
        // {{{
        //    object Box {
        //      implicit def anyToBox[T](t: T): Box[T] = new Box(t)
        //    }
        //    class Box[T](val t: T)
        // }}}
        // We don't want the implicit conversion from Box[T] to Box[Box[T]] to appear.
        return Nil
      }

      // type the view application so we get the exact type of the result (not the formal type)
      val viewTree = result.tree.setType(viewSimplifiedType)
      val appliedTree = new ApplyImplicitView(viewTree, List(Ident("<argument>") setType viewTree.tpe.paramTypes.head))
      val appliedTreeTyped: Tree = {
        val newContext = context.makeImplicit(context.ambiguousErrors)
        newContext.macrosEnabled = false
        val newTyper = global.analyzer.newTyper(newContext)
          newTyper.silent(_.typed(appliedTree), reportAmbiguousErrors = false) match {

          case global.analyzer.SilentResultValue(t: Tree) => t
          case global.analyzer.SilentTypeError(err) =>
            global.reporter.warning(sym.pos, err.toString)
            return Nil
        }
      }

      // now we have the final type:
      val toType = wildcardToNothing(typeVarToOriginOrWildcard(appliedTreeTyped.tpe.finalResultType))

      try {
        // Transform bound constraints into scaladoc constraints
        val implParamConstraints = makeImplicitConstraints(viewImplicitTypes, sym, context, inTpl)
        val boundsConstraints = makeBoundedConstraints(sym.typeParams, constrs, inTpl)
        // TODO: no substitution constraints appear in the library and compiler scaladoc. Maybe they can be removed?
        val substConstraints = makeSubstitutionConstraints(result.subst, inTpl)
        val constraints = implParamConstraints ::: boundsConstraints ::: substConstraints

        List(new ImplicitConversionImpl(sym, result.tree.symbol, toType, constraints, inTpl))
      } catch {
        case i: ImplicitNotFound =>
          //debug(s"  Eliminating: $toType")
          Nil
      }
    }

  def makeImplicitConstraints(types: List[Type], sym: Symbol, context: Context, inTpl: DocTemplateImpl): List[Constraint] =
    types.flatMap((tpe:Type) => {
      // TODO: Before creating constraints, map typeVarToOriginOrWildcard on the implicitTypes
      val implType = typeVarToOriginOrWildcard(tpe)
      val qualifiedName = makeQualifiedName(implType.typeSymbol)

      var available: Option[Boolean] = None

      // see: https://groups.google.com/forum/?hl=en&fromgroups#!topic/scala-internals/gm_fr0RKzC4
      //
      // println(implType + " => " + implType.isTrivial)
      // var tpes: List[Type] = List(implType)
      // while (!tpes.isEmpty) {
      //   val tpe = tpes.head
      //   tpes = tpes.tail
      //   tpe match {
      //     case TypeRef(pre, sym, args) =>
      //       tpes = pre :: args ::: tpes
      //       println(tpe + " => " + tpe.isTrivial)
      //     case _ =>
      //       println(tpe + " (of type" + tpe.getClass + ") => " + tpe.isTrivial)
      //   }
      // }
      // println("\n")

      // look for type variables in the type. If there are none, we can decide if the implicit is there or not
      if (implType.isTrivial) {
        try {
          // TODO: Not sure if `owner = sym.owner` is the right thing to do -- seems similar to what scalac should be doing
          val silentContext = context.make(owner = sym.owner).makeSilent(reportAmbiguousErrors = false)
          val search = inferImplicitByTypeSilent(tpe, silentContext)
          available = Some(search.tree != EmptyTree)
        } catch {
          case _: TypeError =>
        }
      }

      available match {
        case Some(true) =>
          Nil
        case Some(false) if !settings.docImplicitsShowAll =>
          // if -implicits-show-all is not set, we get rid of impossible conversions (such as Numeric[String])
          throw new ImplicitNotFound(implType)
        case _ =>
          val typeParamNames = sym.typeParams.map(_.name)

          // TODO: This is maybe the worst hack I ever did - it's as dirty as hell, but it seems to work, so until I
          // learn more about symbols, it'll have to do.
          implType match {
            case TypeRef(pre, sym, List(TypeRef(NoPrefix, targ, Nil))) if (typeParamNames contains targ.name) =>
              hardcoded.knownTypeClasses.get(qualifiedName) match {
                case Some(explanation) =>
                  List(new KnownTypeClassConstraint {
                    val typeParamName = targ.nameString
                    lazy val typeExplanation = explanation
                    lazy val typeClassEntity = makeTemplate(sym)
                    lazy val implicitType: TypeEntity = makeType(implType, inTpl)
                  })
                case None =>
                  List(new TypeClassConstraint {
                    val typeParamName = targ.nameString
                    lazy val typeClassEntity = makeTemplate(sym)
                    lazy val implicitType: TypeEntity = makeType(implType, inTpl)
                  })
              }
            case _ =>
              List(new ImplicitInScopeConstraint{
                lazy val implicitType: TypeEntity = makeType(implType, inTpl)
              })
          }
      }
    })

  def makeSubstitutionConstraints(subst: TreeTypeSubstituter, inTpl: DocTemplateImpl): List[Constraint] =
    (subst.from zip subst.to) map {
      case (from, to) =>
        new EqualTypeParamConstraint {
          error("Scaladoc implicits: Unexpected type substitution constraint from: " + from + " to: " + to)
          val typeParamName = from.toString
          val rhs = makeType(to, inTpl)
        }
    }

  def makeBoundedConstraints(tparams: List[Symbol], constrs: List[TypeConstraint], inTpl: DocTemplateImpl): List[Constraint] =
    (tparams zip constrs) flatMap {
      case (tparam, constr) => {
        uniteConstraints(constr) match {
          case (loBounds, upBounds) => (loBounds filter (_ != NothingTpe), upBounds filter (_ != AnyTpe)) match {
            case (Nil, Nil) =>
              Nil
            case (List(lo), List(up)) if (lo == up) =>
              List(new EqualTypeParamConstraint {
                val typeParamName = tparam.nameString
                lazy val rhs = makeType(lo, inTpl)
              })
            case (List(lo), List(up)) =>
              List(new BoundedTypeParamConstraint {
                val typeParamName = tparam.nameString
                lazy val lowerBound = makeType(lo, inTpl)
                lazy val upperBound = makeType(up, inTpl)
              })
            case (List(lo), Nil) =>
              List(new LowerBoundedTypeParamConstraint {
                val typeParamName = tparam.nameString
                lazy val lowerBound = makeType(lo, inTpl)
              })
            case (Nil, List(up)) =>
              List(new UpperBoundedTypeParamConstraint {
                val typeParamName = tparam.nameString
                lazy val upperBound = makeType(up, inTpl)
              })
            case other =>
              // this is likely an error on the lub/glb side
              error("Scaladoc implicits: Error computing lub/glb for: " + ((tparam, constr)) + ":\n" + other)
              Nil
          }
        }
      }
    }

  /* ============== IMPLEMENTATION PROVIDING ENTITY TYPES ============== */

  class ImplicitConversionImpl(
    val sym: Symbol,
    val convSym: Symbol,
    val toType: Type,
    val constrs: List[Constraint],
    inTpl: DocTemplateImpl)
      extends ImplicitConversion {

    def source: DocTemplateEntity = inTpl

    def targetType: TypeEntity = makeType(toType, inTpl)

    def convertorOwner: TemplateEntity = {
      if (convSym eq NoSymbol)
        error("Scaladoc implicits: " + toString + " = NoSymbol!")

      makeTemplate(convSym.owner)
    }

    def targetTypeComponents: List[(TemplateEntity, TypeEntity)] = makeParentTypes(toType, None, inTpl)

    def convertorMethod: Either[MemberEntity, String] = {
      var convertor: MemberEntity = null

      convertorOwner match {
        case doc: DocTemplateImpl =>
          val convertors = members.collect { case m: MemberImpl if m.sym == convSym => m }
          if (convertors.length == 1)
            convertor = convertors.head
        case _ =>
      }
      if (convertor ne null)
        Left(convertor)
      else
        Right(convSym.nameString)
    }

    def conversionShortName = convSym.nameString

    def conversionQualifiedName = makeQualifiedName(convSym)

    lazy val constraints: List[Constraint] = constrs

    lazy val memberImpls: List[MemberImpl] = {
      // Obtain the members inherited by the implicit conversion
      val memberSyms = toType.members.filter(implicitShouldDocument(_)).toList

      // Debugging part :)
      debug(sym.nameString + "\n" + "=" * sym.nameString.length())
      debug(" * conversion " + convSym + " from " + sym.tpe + " to " + toType)

      debug("   -> full type: " + toType)
      if (constraints.length != 0) {
        debug("   -> constraints: ")
        constraints foreach { constr => debug("      - " + constr) }
      }
      debug("   -> members:")
      memberSyms foreach (sym => debug("      - "+ sym.decodedName +" : " + sym.info))
      debug("")

      memberSyms.flatMap({ aSym =>
        // we can't just pick up nodes from the original template, although that would be very convenient:
        // they need the byConversion field to be attached to themselves and the types to be transformed by
        // asSeenFrom

        // at the same time, the member itself is in the inTpl, not in the new template -- but should pick up
        // variables from the old template. Ugly huh? We'll always create the member inTpl, but it will change
        // the template when expanding variables in the comment :)
        makeMember(aSym, Some(this), inTpl)
      })
    }

    lazy val members: List[MemberEntity] = memberImpls

    def isHiddenConversion = settings.hiddenImplicits(conversionQualifiedName)

    override def toString = "Implicit conversion from " + sym.tpe + " to " + toType + " done by " + convSym
  }

  /* ========================= HELPER METHODS ========================== */
  /**
   *  Computes the shadowing table for all the members in the implicit conversions
   *  @param members All template's members, including usecases and full signature members
   *  @param convs All the conversions the template takes part in
   *  @param inTpl the usual :)
   */
  def makeShadowingTable(members: List[MemberImpl],
                         convs: List[ImplicitConversionImpl],
                         inTpl: DocTemplateImpl): Map[MemberEntity, ImplicitMemberShadowing] = {
    assert(modelFinished)

    val shadowingTable = mutable.Map[MemberEntity, ImplicitMemberShadowing]()
    val membersByName: Map[Name, List[MemberImpl]] = members.groupBy(_.sym.name)
    val convsByMember = (Map.empty[MemberImpl, ImplicitConversionImpl] /: convs) {
      case (map, conv) => map ++ conv.memberImpls.map (_ -> conv)
    }

    for (conv <- convs) {
      val otherConvMembers: Map[Name, List[MemberImpl]] = convs filterNot (_ == conv) flatMap (_.memberImpls) groupBy (_.sym.name)

      for (member <- conv.memberImpls) {
        val sym1 = member.sym
        val tpe1 = conv.toType.memberInfo(sym1)

        // check if it's shadowed by a member in the original class.
        val shadowed = membersByName.get(sym1.name).toList.flatten filter { other =>
          !settings.docImplicitsSoundShadowing.value || !isDistinguishableFrom(tpe1, inTpl.sym.info.memberInfo(other.sym))
        }

        // check if it's shadowed by another conversion.
        val ambiguous = otherConvMembers.get(sym1.name).toList.flatten filter { other =>
          val tpe2 = convsByMember(other).toType.memberInfo(other.sym)
          !isDistinguishableFrom(tpe1, tpe2) || !isDistinguishableFrom(tpe2, tpe1)
        }

        // we finally have the shadowing info
        if (!shadowed.isEmpty || !ambiguous.isEmpty) {
          val shadowing = new ImplicitMemberShadowing {
            def shadowingMembers: List[MemberEntity] = shadowed
            def ambiguatingMembers: List[MemberEntity] = ambiguous
          }

          shadowingTable += (member -> shadowing)
        }
      }
    }

    shadowingTable.toMap
  }


  /**
   * uniteConstraints takes a TypeConstraint instance and simplifies the constraints inside
   *
   * Normally TypeConstraint contains multiple lower and upper bounds, and we want to reduce this to a lower and an
   * upper bound. Here are a couple of catches we need to be aware of:
   *  - before finding a view (implicit method in scope that maps class A[T1,T2,.. Tn] to something else) the type
   * parameters are transformed into "untouchable" type variables so that type inference does not attempt to
   * fully solve them down to a type but rather constrains them on both sides just enough for the view to be
   * applicable -- now, we want to transform those type variables back to the original type parameters
   *  - some of the bounds fail type inference and therefore refer to Nothing => when performing unification (lub, glb)
   * they start looking ugly => we (unsoundly) transform Nothing to WildcardType so we fool the unification algorithms
   * into thinking there's nothing there
   *  - we don't want the wildcard types surviving the unification so we replace them back to Nothings
   */
  def uniteConstraints(constr: TypeConstraint): (List[Type], List[Type]) =
    try {
      (List(wildcardToNothing(lub(constr.loBounds map typeVarToOriginOrWildcard))),
       List(wildcardToNothing(glb(constr.hiBounds map typeVarToOriginOrWildcard))))
    } catch {
      // does this actually ever happen? (probably when type vars occur in the bounds)
      case x: Throwable => (constr.loBounds.distinct, constr.hiBounds.distinct)
    }

  /**
   *  Make implicits explicit - Not used currently
   */
  // object implicitToExplicit extends TypeMap {
  //   def apply(tp: Type): Type = mapOver(tp) match {
  //     case MethodType(params, resultType) =>
  //       MethodType(params.map(param => if (param.isImplicit) param.cloneSymbol.resetFlag(Flags.IMPLICIT) else param), resultType)
  //     case other =>
  //       other
  //   }
  // }

  /**
   * removeImplicitParameters transforms implicit parameters from the view result type into constraints and
   * returns the simplified type of the view
   *
   * for the example view:
   *   implicit def enrichMyClass[T](a: MyClass[T])(implicit ev: Numeric[T]): EnrichedMyClass[T]
   * the implicit view result type is:
   *   (a: MyClass[T])(implicit ev: Numeric[T]): EnrichedMyClass[T]
   * and the simplified type will be:
   *   MyClass[T] => EnrichedMyClass[T]
   */
  def removeImplicitParameters(viewType: Type): (Type, List[Type]) = {

    val params = viewType.paramss.flatten
    val (normalParams, implParams) = params.partition(!_.isImplicit)
    val simplifiedType = MethodType(normalParams, viewType.finalResultType)
    val implicitTypes = implParams.map(_.tpe)

    (simplifiedType, implicitTypes)
  }

  /**
   * typeVarsToOriginOrWildcard transforms the "untouchable" type variables into either their origins (the original
   * type parameters) or into wildcard types if nothing matches
   */
  object typeVarToOriginOrWildcard extends TypeMap {
    def apply(tp: Type): Type = mapOver(tp) match {
      case tv: TypeVar =>
        if (tv.constr.inst.typeSymbol == NothingClass)
          WildcardType
        else
          tv.origin //appliedType(tv.origin.typeConstructor, tv.typeArgs map this)
      case other =>
        if (other.typeSymbol == NothingClass)
          WildcardType
        else
          other
    }
  }

  /**
   * wildcardToNothing transforms wildcard types back to Nothing
   */
  object wildcardToNothing extends TypeMap {
    def apply(tp: Type): Type = mapOver(tp) match {
      case WildcardType =>
        NothingTpe
      case other =>
        other
    }
  }

  /** implicitShouldDocument decides whether a member inherited by implicit conversion should be documented */
  def implicitShouldDocument(aSym: Symbol): Boolean = {
    // We shouldn't document:
    // - constructors
    // - common methods (in Any, AnyRef, Object) as they are automatically removed
    // - private and protected members (not accessible following an implicit conversion)
    // - members starting with _ (usually reserved for internal stuff)
    localShouldDocument(aSym) && (!aSym.isConstructor) && (aSym.owner != AnyValClass) &&
    (aSym.owner != AnyClass) && (aSym.owner != ObjectClass) &&
    (!aSym.isProtected) && (!aSym.isPrivate) && (!aSym.name.startsWith("_")) &&
    (aSym.isMethod || aSym.isGetter || aSym.isSetter) &&
    (aSym.nameString != "getClass")
  }

  /* To put it very bluntly: checks if you can call implicitly added method with t1 when t2 is already there in the
   * class. We suppose the name of the two members coincides
   *
   * The trick here is that the resultType does not matter - the condition for removal it that paramss have the same
   * structure (A => B => C may not override (A, B) => C) and that all the types involved are
   * of the implicit conversion's member are subtypes of the parent members' parameters */
  def isDistinguishableFrom(t1: Type, t2: Type): Boolean = {
    // Vlad: I tried using matches but it's not exactly what we need:
    // (p: AnyRef)AnyRef matches ((t: String)AnyRef returns false -- but we want that to be true
    // !(t1 matches t2)
    if (t1.paramss.map(_.length) == t2.paramss.map(_.length)) {
      for ((t1p, t2p) <- t1.paramss.flatten zip t2.paramss.flatten)
       if (!isSubType(t1 memberInfo t1p, t2 memberInfo t2p))
         return true // if on the corresponding parameter you give a type that is in t1 but not in t2
                     // def foo(a: Either[Int, Double]): Int = 3
                     // def foo(b: Left[T1]): Int = 6
                     // a.foo(Right(4.5d)) prints out 3 :)
      false
    } else true // the member structure is different foo(3, 5) vs foo(3)(5)
  }
}
