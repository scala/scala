/*  NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author
 */

package scala.tools.nsc
package transform

import scala.collection.mutable
import scala.reflect.internal.util.ListOfNil
import symtab.Flags._

/** This phase converts classes with parameters into Java-like classes with
 *  fields, which are assigned to from constructors.
 */
abstract class Constructors extends Statics with Transform with TypingTransformers with ast.TreeDSL {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "constructors"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ConstructorTransformer(unit)

  private val guardedCtorStats: mutable.Map[Symbol, List[Tree]] = perRunCaches.newMap[Symbol, List[Tree]]()
  private val ctorParams: mutable.Map[Symbol, List[Symbol]] = perRunCaches.newMap[Symbol, List[Symbol]]()

  class ConstructorTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    /*
     * Inspect for obvious out-of-order initialization; concrete, eager vals or vars, declared in this class,
     * for which a reference to the member precedes its definition.
     */
    private def checkUninitializedReads(cd: ClassDef) {
      val stats = cd.impl.body
      val clazz = cd.symbol

      def checkableForInit(sym: Symbol) = (
           (sym ne null)
        && (sym.isVal || sym.isVar)
        && !(sym hasFlag LAZY | DEFERRED | SYNTHETIC)
      )
      val uninitializedVals = mutable.Set[Symbol](
        stats collect { case vd: ValDef if checkableForInit(vd.symbol) => vd.symbol.accessedOrSelf }: _*
      )
      if (uninitializedVals.size > 1)
        log("Checking constructor for init order issues among: " + uninitializedVals.toList.map(_.name.toString.trim).distinct.sorted.mkString(", "))

      for (stat <- stats) {
        // Checking the qualifier symbol is necessary to prevent a selection on
        // another instance of the same class from potentially appearing to be a forward
        // reference on the member in the current class.
        def check(tree: Tree) = {
          for (t <- tree) t match {
            case t: RefTree if uninitializedVals(t.symbol.accessedOrSelf) && t.qualifier.symbol == clazz =>
              reporter.warning(t.pos, s"Reference to uninitialized ${t.symbol.accessedOrSelf}")
            case _ =>
          }
        }
        stat match {
          case vd: ValDef      =>
            // doing this first allows self-referential vals, which to be a conservative
            // warner we will do because it's possible though difficult for it to be useful.
            uninitializedVals -= vd.symbol.accessedOrSelf
            if (!vd.symbol.isLazy)
              check(vd.rhs)
          case _: MemberDef    => // skip other member defs
          case t               => check(t) // constructor body statement
        }
      }

    } // end of checkUninitializedReads()

    override def transform(tree: Tree): Tree = {
      tree match {
        case cd @ ClassDef(mods0, name0, tparams0, impl0) if !isPrimitiveValueClass(cd.symbol) && cd.symbol.primaryConstructor != NoSymbol =>
          if(cd.symbol eq AnyValClass) {
            cd
          }
          else {
            checkUninitializedReads(cd)
            val tplTransformer = new TemplateTransformer(unit, impl0)
            tplTransformer.localTyper = this.localTyper
            tplTransformer.atOwner(impl0, cd.symbol) {
              treeCopy.ClassDef(cd, mods0, name0, tparams0, tplTransformer.transformed)
            }
          }
        case _ =>
          super.transform(tree)
      }
    }

  } // ConstructorTransformer

  /*
   * Summary
   * -------
   *
   * The following gets elided unless they're actually needed:
   *   (a) parameter-accessor fields for non-val, non-var, constructor-param-symbols, as well as
   *   (b) outer accessors of a final class which don't override anything.
   *
   *
   * Gory details
   * ------------
   *
   * The constructors phase elides
   *
   *  (a) parameter-accessor fields for non-val, non-var, constructor-param-symbols
   *      provided they're only accessed within the primary constructor;
   *
   * as well as
   *
   *  (b) outer accessors directly owned by the class of interest,
   *      provided that class is final, they don't override anything, and moreover they aren't accessed anywhere.
   *      An outer accessor is backed by a param-accessor field.
   *      If an outer-accessor can be elided then its supporting field can be elided as well.
   *
   * Once the potential candidates for elision are known (as described above) it remains to visit
   * those program locations where they might be accessed, and only those.
   *
   * What trees can be visited at this point?
   * To recap, by the time the constructors phase runs, local definitions have been hoisted out of their original owner.
   * Moreover, by the time elision is about to happen, the `intoConstructors` rewriting
   * of template-level statements has taken place (the resulting trees can be found in `constructorStats`).
   *
   * That means:
   *
   *   - nested classes are to be found in `defs`
   *
   *   - value and method definitions are also in `defs` and none of them contains local methods or classes.
   *
   *   - auxiliary constructors are to be found in `auxConstructors`
   *
   * Coming back to the question which trees may contain accesses:
   *
   *   (c) regarding parameter-accessor fields, all candidates in (a) are necessarily private-local,
   *       and thus may only be accessed from value or method definitions owned by the current class
   *       (ie there's no point drilling down into nested classes).
   *
   *   (d) regarding candidates in (b), they are accessible from all places listed in (c) and in addition
   *       from nested classes (nested at any number of levels).
   *
   * In all cases, we're done with traversing as soon as all candidates have been ruled out.
   *
   * Finally, the whole affair of eliding is avoided for DelayedInit subclasses,
   * given that for them usually nothing gets elided anyway.
   * That's a consequence from re-locating the post-super-calls statements from their original location
   * (the primary constructor) into a dedicated synthetic method that an anon-closure may invoke, as required by DelayedInit.
   *
   */
  private trait OmittablesHelper {
    def computeOmittableAccessors(clazz: Symbol, defs: List[Tree], auxConstructors: List[Tree]): Set[Symbol] = {
      val decls = clazz.info.decls.toSet
      val isEffectivelyFinal = clazz.isEffectivelyFinal

      // Initially populated with all elision candidates.
      // Trees are traversed, and those candidates are removed which are actually needed.
      // After that, `omittables` doesn't shrink anymore: each symbol it contains can be unlinked from clazz.info.decls.
      //
      // Note: elision of outer reference is based on a class-wise analysis, if a class might have subclasses,
      //       it doesn't work. For example, `LocalParent` retains the outer reference in:
      //
      //   class Outer { def test = {class LocalParent; class LocalChild extends LocalParent } }
      //
      // See run/t9408.scala for related test cases.
      def omittableParamAcc(sym: Symbol) = sym.isParamAccessor && sym.isPrivateLocal
      def omittableOuterAcc(sym: Symbol) = isEffectivelyFinal && sym.isOuterAccessor && !sym.isOverridingSymbol
      val omittables = mutable.Set.empty[Symbol] ++ (decls filter (sym => omittableParamAcc(sym) || omittableOuterAcc(sym))) // the closure only captures isEffectivelyFinal

      // no point traversing further once omittables is empty, all candidates ruled out already.
      object detectUsages extends Traverser {
        lazy val bodyOfOuterAccessor = defs.collect{ case dd: DefDef if omittableOuterAcc(dd.symbol) => dd.symbol -> dd.rhs }.toMap

        override def traverse(tree: Tree): Unit =
          if (omittables.nonEmpty) {
            def sym = tree.symbol
            tree match {
              case _: DefDef if (sym.owner eq clazz) && omittableOuterAcc(sym) => // don't mark as "needed" the field supporting this outer-accessor (not just yet)
              case _: Select if omittables(sym) => omittables -= sym // mark usage
                bodyOfOuterAccessor get sym foreach traverse // recurse to mark as needed the field supporting the outer-accessor-method
                super.traverse(tree)
              case _ => super.traverse(tree)
            }
          }
      }

      if (omittables.nonEmpty)
        (defs.iterator ++ auxConstructors.iterator) foreach detectUsages.traverse

      omittables.toSet
    }
  } // OmittablesHelper

  trait ConstructorTransformerBase {
    def unit: CompilationUnit
    def impl: Template
    def clazz: Symbol
    def localTyper: analyzer.Typer
  }

  /*
   *  TemplateTransformer rewrites DelayedInit subclasses.
   *  The list of statements that will end up in the primary constructor can be split into:
   *
   *    (a) up to and including the super-constructor call.
   *        These statements can occur only in the (bytecode-level) primary constructor.
   *
   *    (b) remaining statements
   *
   *  The purpose of DelayedInit is leaving (b) out of the primary constructor and have their execution "delayed".
   *
   *  The rewriting to achieve "delayed initialization" involves:
   *    (c) an additional, synthetic, public method encapsulating (b)
   *    (d) an additional, synthetic closure whose argless apply() just invokes (c)
   *    (e) after executing the statements in (a),
   *        the primary constructor instantiates (d) and passes it as argument
   *        to a `delayedInit()` invocation on the current instance.
   *        In turn, `delayedInit()` is a method defined as abstract in the `DelayedInit` trait
   *        so that it can be overridden (for an example see `scala.App`)
   *
   *  The following helper methods prepare Trees as part of this rewriting:
   *
   *    (f) `delayedEndpointDef()` prepares (c).
   *        A transformer, `constrStatTransformer`, is used to re-locate statements (b) from template-level
   *        to become statements in method (c). The main task here is re-formulating accesses to params
   *        of the primary constructors (to recap, (c) has zero-params) in terms of param-accessor fields.
   *        In a Delayed-Init subclass, each class-constructor gets a param-accessor field because `mustbeKept()` forces it.
   *
   *    (g) `delayedInitClosure()` prepares (d)
   *
   *    (h) `delayedInitCall()`    prepares the `delayedInit()` invocation referred to in (e)
   *
   *  Both (c) and (d) are added to the Template returned by `transformClassTemplate()`
   *
   *  A note of historic interest: Previously the rewriting for DelayedInit would include in the closure body
   *  all of the delayed initialization sequence, which in turn required:
   *    - reformulating "accesses-on-this" into "accesses-on-outer", and
   *    - adding public getters and setters.
   *
   *  @param stats the statements in (b) above
   *
   *  @return the DefDef for (c) above
   *
   * */
  private trait DelayedInitHelper extends ConstructorTransformerBase {
    private def delayedEndpointDef(stats: List[Tree]): DefDef = {
      val methodName = currentUnit.freshTermName("delayedEndpoint$" + clazz.fullNameAsName('$').toString + "$")
      val methodSym  = clazz.newMethod(methodName, impl.pos, SYNTHETIC | FINAL)
      methodSym setInfoAndEnter MethodType(Nil, UnitTpe)

      // changeOwner needed because the `stats` contained in the DefDef were owned by the template, not long ago.
      val blk       = Block(stats, gen.mkZero(UnitTpe)).changeOwner(impl.symbol -> methodSym)
      val delayedDD = localTyper typed { DefDef(methodSym, Nil, blk) }

      delayedDD.asInstanceOf[DefDef]
    }

    private def delayedInitClosure(delayedEndPointSym: MethodSymbol): ClassDef = {
      val satelliteClass = localTyper.typed {
        atPos(impl.pos) {
          val closureClass   = clazz.newClass(nme.delayedInitArg.toTypeName, impl.pos, SYNTHETIC | FINAL)
          val closureParents = List(AbstractFunctionClass(0).tpe)

          closureClass setInfoAndEnter new ClassInfoType(closureParents, newScope, closureClass)

          val outerField: TermSymbol = (
            closureClass
              newValue(nme.OUTER, impl.pos, PrivateLocal | PARAMACCESSOR)
              setInfoAndEnter clazz.tpe
          )
          val applyMethod: MethodSymbol = (
            closureClass
              newMethod(nme.apply, impl.pos, FINAL)
              setInfoAndEnter MethodType(Nil, ObjectTpe)
          )
          val outerFieldDef     = ValDef(outerField)
          val closureClassTyper = localTyper.atOwner(closureClass)
          val applyMethodTyper  = closureClassTyper.atOwner(applyMethod)

          def applyMethodStat =
            applyMethodTyper.typed {
              atPos(impl.pos) {
                val receiver = Select(This(closureClass), outerField)
                Apply(Select(receiver, delayedEndPointSym), Nil)
              }
            }

          val applyMethodDef = DefDef(
            sym = applyMethod,
            vparamss = ListOfNil,
            rhs = Block(applyMethodStat, gen.mkAttributedRef(BoxedUnit_UNIT)))

          ClassDef(
            sym = closureClass,
            constrMods = Modifiers(0),
            vparamss = List(List(outerFieldDef)),
            body = applyMethodDef :: Nil,
            superPos = impl.pos)
        }
      }

      satelliteClass.asInstanceOf[ClassDef]
    }

    /** For a DelayedInit subclass, wrap remainingConstrStats into a DelayedInit closure. */
    def delayedInitDefsAndConstrStats(defs: List[Tree], remainingConstrStats: List[Tree]): (List[Tree], List[Tree]) = {
      val delayedHook     = delayedEndpointDef(remainingConstrStats)
      val delayedHookSym  = delayedHook.symbol.asInstanceOf[MethodSymbol]

      // transform to make the closure-class' default constructor assign the outer instance to its param-accessor field.
      val hookCallerClass = (new ConstructorTransformer(unit)) transform delayedInitClosure(delayedHookSym)
      val delayedInitCall = localTyper.typedPos(impl.pos) {
        gen.mkMethodCall(This(clazz), delayedInitMethod, Nil, List(New(hookCallerClass.symbol.tpe, This(clazz))))
      }

      (List(delayedHook, hookCallerClass), List(delayedInitCall))
    }

  } // DelayedInitHelper

  private trait GuardianOfCtorStmts extends ConstructorTransformerBase {
    def primaryConstrParams: List[Symbol]
    def usesSpecializedField: Boolean

    lazy val hasSpecializedFieldsSym   = clazz.info.decl(nme.SPECIALIZED_INSTANCE)
    // The constructor of a non-specialized class that has specialized subclasses
    // should use `q"${hasSpecializedFieldsSym}()"` to guard the initialization of specialized fields.
    lazy val guardSpecializedFieldInit = (hasSpecializedFieldsSym != NoSymbol) && !clazz.hasFlag(SPECIALIZED)

    /* Return a single list of statements, merging the generic class constructor with the
     * specialized stats. The original statements are retyped in the current class, and
     * assignments to generic fields that have a corresponding specialized assignment in
     * `specializedStats` are replaced by the specialized assignment.
     */
    private def mergeConstructors(genericClazz: Symbol, originalStats: List[Tree], specializedStats: List[Tree]): List[Tree] = {
      val specBuf = new mutable.ListBuffer[Tree]
      specBuf ++= specializedStats

      def specializedAssignFor(sym: Symbol): Option[Tree] =
        specializedStats find {
          case Assign(sel @ Select(This(_), _), _) =>
            sel.symbol.isSpecialized && (nme.unspecializedName(sel.symbol.getterName) == sym.getterName)
          case _ => false
        }

      /* Rewrite calls to ScalaRunTime.array_update to the proper apply method in scala.Array.
       * Erasure transforms Array.update to ScalaRunTime.update when the element type is a type
       * variable, but after specialization this is a concrete primitive type, so it would
       * be an error to pass it to array_update(.., .., Object).
       */
      def rewriteArrayUpdate(tree: Tree): Tree = {
        val arrayUpdateMethod = currentRun.runDefinitions.arrayUpdateMethod
        val adapter = new Transformer {
          override def transform(t: Tree): Tree = t match {
            case Apply(fun @ Select(receiver, method), List(xs, idx, v)) if fun.symbol == arrayUpdateMethod =>
              localTyper.typed(Apply(gen.mkAttributedSelect(xs, arrayUpdateMethod), List(idx, v)))
            case _ => super.transform(t)
          }
        }
        adapter.transform(tree)
      }

      log("merging: " + originalStats.mkString("\n") + "\nwith\n" + specializedStats.mkString("\n"))
      for (s <- originalStats; stat = s.duplicate) yield {
        log("merge: looking at " + stat)
        val stat1 = stat match {
          case Assign(sel @ Select(This(_), field), _) =>
            specializedAssignFor(sel.symbol).getOrElse(stat)
          case _ => stat
        }
        if (stat1 ne stat) {
          log("replaced " + stat + " with " + stat1)
          specBuf -= stat1
        }

        if (stat1 eq stat) {
          assert(ctorParams(genericClazz).length == primaryConstrParams.length)
          // this is just to make private fields public
          (new specializeTypes.ImplementationAdapter(ctorParams(genericClazz), primaryConstrParams, null, true))(stat1)

          val stat2 = rewriteArrayUpdate(stat1)
          // statements coming from the original class need retyping in the current context
          debuglog("retyping " + stat2)

          val d = new specializeTypes.Duplicator(Map[Symbol, Type]())
          d.retyped(localTyper.context1.asInstanceOf[d.Context],
                    stat2,
                    genericClazz,
                    clazz,
                    Map.empty)
        } else
          stat1
      }
//      if (specBuf.nonEmpty)
//        println("residual specialized constructor statements: " + specBuf)
    }

    /* Add an 'if' around the statements coming after the super constructor. This
     * guard is necessary if the code uses specialized fields. A specialized field is
     * initialized in the subclass constructor, but the accessors are (already) overridden
     * and pointing to the (empty) fields. To fix this, a class with specialized fields
     * will not run its constructor statements if the instance is specialized. The specialized
     * subclass includes a copy of those constructor statements, and runs them. To flag that a class
     * has specialized fields, and their initialization should be deferred to the subclass, method
     * 'specInstance$' is added in phase specialize.
     */
    def guardSpecializedInitializer(stats: List[Tree]): List[Tree] = if (settings.nospecialization.value) stats else {
      // // split the statements in presuper and postsuper
      // var (prefix, postfix) = stats0.span(tree => !((tree.symbol ne null) && tree.symbol.isConstructor))
      // if (postfix.nonEmpty) {
      //   prefix = prefix :+ postfix.head
      //   postfix = postfix.tail
      // }

      if (guardSpecializedFieldInit && usesSpecializedField && stats.nonEmpty) {
        // save them for duplication in the specialized subclass
        guardedCtorStats(clazz) = stats
        ctorParams(clazz) = primaryConstrParams

        val tree =
          If(
            Apply(
              CODE.NOT (
               Apply(gen.mkAttributedRef(hasSpecializedFieldsSym), List())),
              List()),
            Block(stats, Literal(Constant(()))),
            EmptyTree)

        List(localTyper.typed(tree))
      }
      else if (clazz.hasFlag(SPECIALIZED)) {
        // add initialization from its generic class constructor
        val genericName  = nme.unspecializedName(clazz.name)
        val genericClazz = clazz.owner.info.decl(genericName.toTypeName)
        assert(genericClazz != NoSymbol, clazz)

        guardedCtorStats.get(genericClazz) match {
          case Some(stats1) => mergeConstructors(genericClazz, stats1, stats)
          case None => stats
        }
      } else stats
    }

  } // GuardianOfCtorStmts

  private class TemplateTransformer(val unit: CompilationUnit, val impl: Template)
    extends TypingTransformer(unit)
    with    StaticsTransformer
    with    DelayedInitHelper
    with    OmittablesHelper
    with    GuardianOfCtorStmts
    with    fields.CheckedAccessorTreeSynthesis
  {
    protected def typedPos(pos: Position)(tree: Tree): Tree = localTyper.typedPos(pos)(tree)

    val clazz         = impl.symbol.owner  // the transformed class

    val isDelayedInitSubclass = clazz isSubClass DelayedInitClass

    private val stats = impl.body          // the transformed template body

    // find and dissect primary constructor
    private val (primaryConstr, _primaryConstrParams, primaryConstrBody) = stats collectFirst {
      case dd@DefDef(_, _, _, vps :: Nil, _, rhs: Block) if dd.symbol.isPrimaryConstructor || dd.symbol.isMixinConstructor => (dd, vps map (_.symbol), rhs)
    } getOrElse {
      abort("no constructor in template: impl = " + impl)
    }


    def primaryConstrParams  = _primaryConstrParams
    def usesSpecializedField = intoConstructor.usesSpecializedField

    // The constructor parameter corresponding to an accessor
    def parameter(acc: Symbol): Symbol = parameterNamed(acc.unexpandedName.getterName)

    // The constructor parameter with given name. This means the parameter
    // has given name, or starts with given name, and continues with a `$` afterwards.
    def parameterNamed(name: Name): Symbol = {
      def matchesName(param: Symbol) = param.name == name || param.name.startsWith(name + nme.NAME_JOIN_STRING)

      primaryConstrParams filter matchesName match {
        case Nil    => abort(name + " not in " + primaryConstrParams)
        case p :: _ => p
      }
    }

    // A transformer for expressions that go into the constructor
    object intoConstructor extends Transformer {
      /*
      * `usesSpecializedField` makes a difference in deciding whether constructor-statements
      * should be guarded in a `guardSpecializedFieldInit` class, ie in a class that's the generic super-class of
      * one or more specialized sub-classes.
      *
      * Given that `usesSpecializedField` isn't read for any other purpose than the one described above,
      * we skip setting `usesSpecializedField` in case the current class isn't `guardSpecializedFieldInit` to start with.
      * That way, trips to a map in `specializeTypes` are saved.
      */
      var usesSpecializedField: Boolean = false

      private def isParamRef(sym: Symbol) = sym.isParamAccessor && sym.owner == clazz

      // Terminology: a stationary location is never written after being read.
      private def isStationaryParamRef(sym: Symbol) = (
        isParamRef(sym) &&
        !(sym.isGetter && sym.accessed.isVariable) &&
        !sym.isSetter
      )

      /*
       * whether `sym` denotes a param-accessor (ie in a class a PARAMACCESSOR field, or in a trait a method with same flag)
       * that fulfills all of:
       *   (a) has stationary value, ie the same value provided via the corresponding ctor-arg; and
       *   (b) isn't subject to specialization. We might be processing statements for:
       *         (b.1) the constructor in the generic   (super-)class; or
       *         (b.2) the constructor in the specialized (sub-)class.
       *   (c) isn't part of a DelayedInit subclass.
       */
      private def canBeSupplanted(sym: Symbol) = !isDelayedInitSubclass && isStationaryParamRef(sym) && !specializeTypes.possiblySpecialized(sym)

      override def transform(tree: Tree): Tree = tree match {
        case Apply(Select(This(_), _), List()) =>
          // references to parameter accessor methods of own class become references to parameters
          // outer accessors become references to $outer parameter
          // println(s"to param ref in $clazz for ${tree.symbol} ${tree.symbol.debugFlagString} / ${tree.symbol.outerSource} / ${canBeSupplanted(tree.symbol)}")
          if (clazz.isTrait && !(tree.symbol hasAllFlags (ACCESSOR | PARAMACCESSOR)))
            super.transform(tree)
          else if (canBeSupplanted(tree.symbol))
            gen.mkAttributedIdent(parameter(tree.symbol)) setPos tree.pos
          else if (tree.symbol.outerSource == clazz && !isDelayedInitSubclass)
            gen.mkAttributedIdent(parameterNamed(nme.OUTER)) setPos tree.pos
          else
            super.transform(tree)

        case Select(This(_), _) if canBeSupplanted(tree.symbol) =>
          // references to parameter accessor field of own class become references to parameters
          gen.mkAttributedIdent(parameter(tree.symbol)) setPos tree.pos

        case Select(_, _) if guardSpecializedFieldInit => // reasoning behind this guard in the docu of `usesSpecializedField`
          if (specializeTypes.possiblySpecialized(tree.symbol)) {
            usesSpecializedField = true
          }
          super.transform(tree)

        case _ =>
          super.transform(tree)
      }

      // Move tree into constructor, take care of changing owner from `oldOwner` to `newOwner` (the primary constructor symbol)
      def apply(oldOwner: Symbol, newOwner: Symbol)(tree: Tree) =
        if (tree eq EmptyTree) tree
        else transform(tree.changeOwner(oldOwner -> newOwner))
    }

    // Assign `rhs` to class field / trait setter `assignSym`
    def mkAssign(assignSym: Symbol, rhs: Tree): Tree =
      localTyper.typedPos(assignSym.pos) {
        val qual = Select(This(clazz), assignSym)
        if (assignSym.isSetter) Apply(qual, List(rhs))
        else Assign(qual, rhs)
      }


    // Create code to copy parameter to parameter accessor field.
    // If parameter is $outer, check that it is not null so that we NPE
    // here instead of at some unknown future $outer access.
    def copyParam(to: Symbol, from: Symbol): Tree = {
      import CODE._
      val result = mkAssign(to, Ident(from))

      if (from.name != nme.OUTER ||
        from.tpe.typeSymbol.isPrimitiveValueClass) result
      else localTyper.typedPos(to.pos) {
        // `throw null` has the same effect as `throw new NullPointerException`, see JVM spec on instruction `athrow`
        IF(from OBJ_EQ NULL) THEN Throw(gen.mkZero(ThrowableTpe)) ELSE result
      }
    }

    /** Triage definitions and statements in this template into the following categories.
      * The primary constructor is treated separately, as it is assembled in part from these pieces.
      *
      * - `defs`:           definitions that go into class
      * - `auxConstrs`:     auxiliary constructors, separate from the defs as they should follow the primary constructor
      * - `constrPrefix`:   early initializer statements that go into constructor before the superclass constructor call
      * - `constrStats`:    statements that go into the constructor after and including the superclass constructor call
      * - `classInitStats`: statements that go into the class initializer
      */
    class Triage {
      private val defBuf, auxConstructorBuf, constrPrefixBuf, constrStatBuf, classInitStatBuf = new mutable.ListBuffer[Tree]

      triage()

      val defs              = defBuf.toList
      val auxConstructors   = auxConstructorBuf.toList
      val constructorPrefix = constrPrefixBuf.toList
      val constructorStats  = constrStatBuf.toList
      val classInitStats    = classInitStatBuf.toList

      private def triage() = {
        // Constant typed vals are not memoized.
        def memoizeValue(sym: Symbol) = !sym.info.resultType.isInstanceOf[ConstantType]

        // The early initialized field definitions of the class (these are the class members)
        val presupers = treeInfo.preSuperFields(stats)

        // generate code to copy pre-initialized fields
        for (stat <- primaryConstrBody.stats) {
          constrStatBuf += stat
          stat match {
            case ValDef(mods, name, _, _) if mods.hasFlag(PRESUPER) => // TODO trait presupers
              // stat is the constructor-local definition of the field value
              val fields = presupers filter (_.getterName == name)
              assert(fields.length == 1, s"expected exactly one field by name $name in $presupers of $clazz's early initializers")
              val to = fields.head.symbol

              if (memoizeValue(to)) constrStatBuf += mkAssign(to, Ident(stat.symbol))
            case _ =>
          }
        }

        val primaryConstrSym = primaryConstr.symbol

        for (stat <- stats) {
          val statSym = stat.symbol

          // Move the RHS of a ValDef to the appropriate part of the ctor.
          // If the val is an early initialized or a parameter accessor,
          // it goes before the superclass constructor call, otherwise it goes after.
          // A lazy val's effect is not moved to the constructor, as it is delayed.
          // Returns `true` when a `ValDef` is needed.
          def moveEffectToCtor(mods: Modifiers, rhs: Tree, assignSym: Symbol): Unit = {
            val initializingRhs =
              if ((assignSym eq NoSymbol) || statSym.isLazy) EmptyTree // not memoized, or effect delayed (for lazy val)
              else if (!mods.hasStaticFlag) intoConstructor(statSym, primaryConstrSym)(rhs)
              else rhs

            if (initializingRhs ne EmptyTree) {
              val initPhase =
                if (mods hasFlag STATIC) classInitStatBuf
                else if (mods hasFlag PRESUPER | PARAMACCESSOR) constrPrefixBuf
                else constrStatBuf

              initPhase += mkAssign(assignSym, initializingRhs)
            }
          }

          stat match {
            // recurse on class definition, store in defBuf
            case _: ClassDef if !statSym.isInterface =>
              defBuf += new ConstructorTransformer(unit).transform(stat)

            // primary constructor is already tracked as `primaryConstr`
            // non-primary constructors go to auxConstructorBuf
            // mixin constructors are suppressed (!?!?)
            case _: DefDef if statSym.isConstructor =>
              if ((statSym ne primaryConstrSym) && !statSym.isMixinConstructor) auxConstructorBuf += stat

            // If a val needs a field, an empty valdef goes into the template.
            // Except for lazy and ConstantTyped vals, the field is initialized by an assignment in:
            //   - the class initializer (static),
            //   - the constructor, before the super call (early initialized or a parameter accessor),
            //   - the constructor, after the super call (regular val).
            case vd: ValDef =>
              if (vd.rhs eq EmptyTree) { defBuf += vd }
              else {
                val emitField = memoizeValue(statSym)

                if (emitField) {
                  moveEffectToCtor(vd.mods, vd.rhs, statSym)
                  defBuf += deriveValDef(stat)(_ => EmptyTree)
                }
              }

            case dd: DefDef =>
              // either move the RHS to ctor (for getter of stored field) or just drop it (for corresponding setter)
              def shouldMoveRHS =
                clazz.isTrait && statSym.isAccessor && !statSym.isLazy && (statSym.isSetter || memoizeValue(statSym))

              if ((dd.rhs eq EmptyTree) || !shouldMoveRHS) { defBuf += dd }
              else {
                if (statSym.isGetter) moveEffectToCtor(dd.mods, dd.rhs, statSym.asTerm.referenced orElse statSym.setterIn(clazz))
                defBuf += deriveDefDef(stat)(_ => EmptyTree)
              }

            // all other statements go into the constructor
            case _ =>
              constrStatBuf += intoConstructor(impl.symbol, primaryConstrSym)(stat)
          }
        }
      }
    }

    def transformed = {
      val triage = new Triage; import triage._

      // omit unused outers
      val omittableAccessor: Set[Symbol] =
        if (isDelayedInitSubclass) Set.empty
        else computeOmittableAccessors(clazz, defs, auxConstructors)

      // TODO: this should omit fields for non-memoized (constant-typed, unit-typed vals need no storage --
      // all the action is in the getter)
      def omittableSym(sym: Symbol) = omittableAccessor(sym)
      def omittableStat(stat: Tree) = omittableSym(stat.symbol)

      // The parameter accessor fields which are members of the class
      val paramAccessors =
        if (clazz.isTrait) clazz.info.decls.toList.filter(sym => sym.hasAllFlags(STABLE | PARAMACCESSOR)) // since a trait does not have constructor parameters (yet), these can only come from lambdalift -- right?
        else clazz.constrParamAccessors

      // Initialize all parameters fields that must be kept.
      val paramInits = paramAccessors filterNot omittableSym map { acc =>
        // Check for conflicting field mixed in for a val/var defined in a parent trait (neg/t1960.scala).
        // Since the fields phase has already mixed in fields, we can just look for
        // an existing decl with the local variant of our paramaccessor's name.
        //
        // TODO: mangle the constructor parameter name (it can only be used internally), though we probably first need more robust name mangling

        // sometimes acc is a field with a local name (when it's a val/var constructor param) --> exclude the `acc` itself when looking for conflicting decl
        // sometimes it's not (just a constructor param) --> any conflicting decl is a problem
        val conflict = clazz.info.decl(acc.name.localName).filter(sym => sym ne acc)
        if (conflict ne NoSymbol) {
          val orig = exitingTyper(clazz.info.nonPrivateMember(acc.name).filter(_ hasFlag ACCESSOR))
          reporter.error(acc.pos, s"parameter '${acc.name}' requires field but conflicts with ${(orig orElse conflict).fullLocationString}")
        }

        val accSetter =
          if (clazz.isTrait) acc.setterIn(clazz, hasExpandedName = true)
          else acc

        copyParam(accSetter, parameter(acc))
      }

      // Return a pair consisting of (all statements up to and including superclass and trait constr calls, rest)
      def splitAtSuper(stats: List[Tree]) = {
        def isConstr(tree: Tree): Boolean = tree match {
          case Block(_, expr) => isConstr(expr) // SI-6481 account for named argument blocks
          case _              => (tree.symbol ne null) && tree.symbol.isConstructor
        }
        val (pre, rest0)       = stats span (!isConstr(_))
        val (supercalls, rest) = rest0 span (isConstr(_))
        (pre ::: supercalls, rest)
      }

      val (uptoSuperStats, remainingConstrStats) = splitAtSuper(constructorStats)

      /* TODO: XXX This condition (`isDelayedInitSubclass && remainingConstrStats.nonEmpty`) is not correct:
      * remainingConstrStats.nonEmpty excludes too much,
      * but excluding it includes too much.  The constructor sequence being mimicked
      * needs to be reproduced with total fidelity.
      *
      * See test case files/run/bug4680.scala, the output of which is wrong in many
      * particulars.
      */
      val (delayedHookDefs, remainingConstrStatsDelayedInit) =
        if (isDelayedInitSubclass && remainingConstrStats.nonEmpty) delayedInitDefsAndConstrStats(defs, remainingConstrStats)
        else (Nil, remainingConstrStats)

      // Assemble final constructor
      val primaryConstructor = deriveDefDef(primaryConstr)(_ => {
        treeCopy.Block(
          primaryConstrBody,
          paramInits ::: constructorPrefix ::: uptoSuperStats ::: guardSpecializedInitializer(remainingConstrStatsDelayedInit),
          primaryConstrBody.expr)
      })

      if (omittableAccessor.exists(_.isOuterField) && !constructorStats.exists(_.exists { case i: Ident if i.symbol.isOuterParam => true; case _ => false}))
        primaryConstructor.symbol.updateAttachment(OuterArgCanBeElided)

      val constructors = primaryConstructor :: auxConstructors

      // Unlink all fields that can be dropped from class scope
      // Iterating on toList is cheaper (decls.filter does a toList anyway)
      val decls = clazz.info.decls
      decls.toList.filter(omittableSym).foreach(decls.unlink)

      // Eliminate all field/accessor definitions that can be dropped from template
      // We never eliminate delayed hooks or the constructors, so, only filter `defs`.
      val prunedStats = (defs filterNot omittableStat) ::: delayedHookDefs ::: constructors

      val statsWithInitChecks =
        if (settings.checkInit) {
          val addChecks = new SynthInitCheckedAccessorsIn(currentOwner)
          prunedStats mapConserve {
            case dd: DefDef => deriveDefDef(dd)(addChecks.wrapRhsWithInitChecks(dd.symbol))
            case stat       => stat
          }
        } else prunedStats

      //  Add the static initializers
      if (classInitStats.isEmpty) deriveTemplate(impl)(_ => statsWithInitChecks)
      else {
        val staticCtor = staticConstructor(statsWithInitChecks, localTyper, impl.pos)(classInitStats)
        deriveTemplate(impl)(_ => staticCtor :: statsWithInitChecks)
      }
    }
  } // TemplateTransformer
}
