/*  NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author
 */

package scala.tools.nsc
package transform

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import symtab.Flags._

/** This phase converts classes with parameters into Java-like classes with
 *  fields, which are assigned to from constructors.
 */
abstract class Constructors extends Transform with ast.TreeDSL {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "constructors"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ConstructorTransformer(unit)

  private val guardedCtorStats: mutable.Map[Symbol, List[Tree]] = perRunCaches.newMap[Symbol, List[Tree]]()
  private val ctorParams: mutable.Map[Symbol, List[Symbol]] = perRunCaches.newMap[Symbol, List[Symbol]]()

  class ConstructorTransformer(unit: CompilationUnit) extends Transformer {

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
      if (uninitializedVals.nonEmpty)
        log("Checking constructor for init order issues among: " + uninitializedVals.map(_.name).mkString(", "))

      for (stat <- stats) {
        // Checking the qualifier symbol is necessary to prevent a selection on
        // another instance of the same class from potentially appearing to be a forward
        // reference on the member in the current class.
        def check(tree: Tree) = {
          for (t <- tree) t match {
            case t: RefTree if uninitializedVals(t.symbol.accessedOrSelf) && t.qualifier.symbol == clazz =>
              unit.warning(t.pos, s"Reference to uninitialized ${t.symbol.accessedOrSelf}")
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
        case cd @ ClassDef(mods0, name0, tparams0, impl0) if !cd.symbol.isInterface && !isPrimitiveValueClass(cd.symbol) =>
          if(cd.symbol eq AnyValClass) {
            cd
          }
          else {
            checkUninitializedReads(cd)
            val impl: Template = (new TemplateTransformer(unit, impl0)).transformed
            treeCopy.ClassDef(cd, mods0, name0, tparams0, impl)
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
   * of template-level statements has taken place (the resulting trees can be found in `constrStatBuf`).
   *
   * That means:
   *
   *   - nested classes are to be found in `defBuf`
   *
   *   - value and method definitions are also in `defBuf` and none of them contains local methods or classes.
   *
   *   - auxiliary constructors are to be found in `auxConstructorBuf`
   *
   * Coming back to the question which trees may contain accesses:
   *
   *   (c) regarding parameter-accessor fields, all candidates in (a) are necessarily private-local,
   *       and thus may only be accessed from value or method definitions owned by the current class
   *       (ie there's no point drilling down into nested classes).
   *
   *   (d) regarding candidates in (b), they are accesible from all places listed in (c) and in addition
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
  private trait OmittablesHelper { self: TemplateTransformer =>

    /*
     * Initially populated with all elision candidates.
     * Trees are traversed, and those candidates are removed which are actually needed.
     * After that, `omittables` doesn't shrink anymore: each symbol it contains can be unlinked from clazz.info.decls.
     */
    val omittables = mutable.Set.empty[Symbol]

    def populateOmittables() {

      omittables.clear()

      if(isDelayedInitSubclass) {
        return
      }

      def isParamCandidateForElision(sym: Symbol) = (sym.isParamAccessor && sym.isPrivateLocal)
      def isOuterCandidateForElision(sym: Symbol) = (sym.isOuterAccessor && sym.owner.isEffectivelyFinal && !sym.isOverridingSymbol)

      val paramCandidatesForElision: Set[ /*Field*/  Symbol] = (clazz.info.decls.toSet filter isParamCandidateForElision)
      val outerCandidatesForElision: Set[ /*Method*/ Symbol] = (clazz.info.decls.toSet filter isOuterCandidateForElision)

      omittables ++= paramCandidatesForElision
      omittables ++= outerCandidatesForElision

      val bodyOfOuterAccessor: Map[Symbol, DefDef] =
        defBuf collect { case dd: DefDef if outerCandidatesForElision(dd.symbol) => dd.symbol -> dd } toMap

      // no point traversing further once omittables is empty, all candidates ruled out already.
      object detectUsages extends Traverser {
        private def markUsage(sym: Symbol) {
          omittables -= debuglogResult("omittables -= ")(sym)
          // recursive call to mark as needed the field supporting the outer-accessor-method.
          bodyOfOuterAccessor get sym foreach (this traverse _.rhs)
        }
        override def traverse(tree: Tree): Unit = if (omittables.nonEmpty) {
          def sym = tree.symbol
          tree match {
            // don't mark as "needed" the field supporting this outer-accessor, ie not just yet.
            case _: DefDef if outerCandidatesForElision(sym) => ()
            case _: Select if omittables(sym)                => markUsage(sym) ; super.traverse(tree)
            case _                                           => super.traverse(tree)
          }
        }
        def walk(xs: Seq[Tree]) = xs.iterator foreach traverse
      }
      if (omittables.nonEmpty) {
        detectUsages walk defBuf
        detectUsages walk auxConstructorBuf
      }
    }
    def mustbeKept(sym: Symbol) = !omittables(sym)

  } // OmittablesHelper

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
  private trait DelayedInitHelper { self: TemplateTransformer =>

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

    private def delayedInitCall(closure: Tree) = localTyper.typedPos(impl.pos) {
      gen.mkMethodCall(This(clazz), delayedInitMethod, Nil, List(New(closure.symbol.tpe, This(clazz))))
    }

    def rewriteDelayedInit() {

      /* XXX This is not corect: remainingConstrStats.nonEmpty excludes too much,
       * but excluding it includes too much.  The constructor sequence being mimicked
       * needs to be reproduced with total fidelity.
       *
       * See test case files/run/bug4680.scala, the output of which is wrong in many
       * particulars.
       */
      val needsDelayedInit = (isDelayedInitSubclass && remainingConstrStats.nonEmpty)

      if (needsDelayedInit) {
        val delayedHook: DefDef = delayedEndpointDef(remainingConstrStats)
        defBuf += delayedHook
        val hookCallerClass = {
          // transform to make the closure-class' default constructor assign the the outer instance to its param-accessor field.
          val drillDown = new ConstructorTransformer(unit)
          drillDown transform delayedInitClosure(delayedHook.symbol.asInstanceOf[MethodSymbol])
        }
        defBuf += hookCallerClass
        remainingConstrStats = delayedInitCall(hookCallerClass) :: Nil
      }

    }

  } // DelayedInitHelper

  private trait GuardianOfCtorStmts { self: TemplateTransformer =>

    /* Return a single list of statements, merging the generic class constructor with the
     * specialized stats. The original statements are retyped in the current class, and
     * assignments to generic fields that have a corresponding specialized assignment in
     * `specializedStats` are replaced by the specialized assignment.
     */
    private def mergeConstructors(genericClazz: Symbol, originalStats: List[Tree], specializedStats: List[Tree]): List[Tree] = {
      val specBuf = new ListBuffer[Tree]
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
      val res = for (s <- originalStats; stat = s.duplicate) yield {
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
          assert(ctorParams(genericClazz).length == constrInfo.constrParams.length)
          // this is just to make private fields public
          (new specializeTypes.ImplementationAdapter(ctorParams(genericClazz), constrInfo.constrParams, null, true))(stat1)

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
      if (specBuf.nonEmpty)
        println("residual specialized constructor statements: " + specBuf)
      res
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

      if (shouldGuard && usesSpecializedField && stats.nonEmpty) {
        // save them for duplication in the specialized subclass
        guardedCtorStats(clazz) = stats
        ctorParams(clazz) = constrInfo.constrParams

        val tree =
          If(
            Apply(
              CODE.NOT (
               Apply(gen.mkAttributedRef(specializedFlag), List())),
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
    extends Transformer
    with    DelayedInitHelper
    with    OmittablesHelper
    with    GuardianOfCtorStmts
    with    JumpOverOuters {

    val clazz = impl.symbol.owner  // the transformed class
    val stats = impl.body          // the transformed template body
    val localTyper = typer.atOwner(impl, clazz)

    val specializedFlag: Symbol = clazz.info.decl(nme.SPECIALIZED_INSTANCE)
    val shouldGuard = (specializedFlag != NoSymbol) && !clazz.hasFlag(SPECIALIZED)

    val isDelayedInitSubclass = (clazz isSubClass DelayedInitClass)

    case class ConstrInfo(
      constr: DefDef,               // The primary constructor
      constrParams: List[Symbol],   // ... and its parameters
      constrBody: Block             // ... and its body
    )
    // decompose primary constructor into the three entities above.
    val constrInfo: ConstrInfo = {
      val ddef = (stats find (_.symbol.isPrimaryConstructor))
      ddef match {
        case Some(ddef @ DefDef(_, _, _, List(vparams), _, rhs @ Block(_, _))) =>
          ConstrInfo(ddef, vparams map (_.symbol), rhs)
        case x =>
          abort("no constructor in template: impl = " + impl)
      }
    }
    import constrInfo._

    // The parameter accessor fields which are members of the class
    val paramAccessors = clazz.constrParamAccessors

    // The constructor parameter corresponding to an accessor
    def parameter(acc: Symbol): Symbol = parameterNamed(acc.unexpandedName.getterName)

    // The constructor parameter with given name. This means the parameter
    // has given name, or starts with given name, and continues with a `$` afterwards.
    def parameterNamed(name: Name): Symbol = {
      def matchesName(param: Symbol) = param.name == name || param.name.startsWith(name + nme.NAME_JOIN_STRING)

      (constrParams filter matchesName) match {
        case Nil    => abort(name + " not in " + constrParams)
        case p :: _ => p
      }
    }

    /*
     * `usesSpecializedField` makes a difference in deciding whether constructor-statements
     * should be guarded in a `shouldGuard` class, ie in a class that's the generic super-class of
     * one or more specialized sub-classes.
     *
     * Given that `usesSpecializedField` isn't read for any other purpose than the one described above,
     * we skip setting `usesSpecializedField` in case the current class isn't `shouldGuard` to start with.
     * That way, trips to a map in `specializeTypes` are saved.
     */
    var usesSpecializedField: Boolean = false

    /*
     * A transformer for expressions that go into the constructor.
     */
    private class IntoCtorTransformer extends Transformer {

      private def isParamRef(sym: Symbol) = (sym.isParamAccessor && sym.owner == clazz)

      /*
       * Terminology: a stationary location is never written after being read.
       */
      private def isStationaryParamRef(sym: Symbol) = (
        isParamRef(sym) &&
        !(sym.isGetter && sym.accessed.isVariable) &&
        !sym.isSetter
      )

      private def possiblySpecialized(s: Symbol) = specializeTypes.specializedTypeVars(s).nonEmpty

      /*
       * whether `sym` denotes a param-accessor (ie a field) that fulfills all of:
       *   (a) has stationary value, ie the same value provided via the corresponding ctor-arg; and
       *   (b) isn't subject to specialization. We might be processing statements for:
       *         (b.1) the constructur in the generic   (super-)class; or
       *         (b.2) the constructor in the specialized (sub-)class.
       *   (c) isn't part of a DelayedInit subclass.
       */
      private def canBeSupplanted(sym: Symbol) = (
        !isDelayedInitSubclass    &&
        isStationaryParamRef(sym) &&
        !possiblySpecialized(sym)
      )

      override def transform(tree: Tree): Tree = tree match {

        case Apply(Select(This(_), _), List()) =>
          // references to parameter accessor methods of own class become references to parameters
          // outer accessors become references to $outer parameter
          if (canBeSupplanted(tree.symbol))
            gen.mkAttributedIdent(parameter(tree.symbol.accessed)) setPos tree.pos
          else if (tree.symbol.outerSource == clazz && !clazz.isImplClass)
            gen.mkAttributedIdent(parameterNamed(nme.OUTER)) setPos tree.pos
          else
            super.transform(tree)

        case Select(This(_), _) if (canBeSupplanted(tree.symbol)) =>
          // references to parameter accessor field of own class become references to parameters
          gen.mkAttributedIdent(parameter(tree.symbol)) setPos tree.pos

        case Select(_, _) if shouldGuard => // reasoning behind this guard in the docu of `usesSpecializedField`
          if (possiblySpecialized(tree.symbol)) {
            usesSpecializedField = true
          }
          super.transform(tree)

        case _ =>
          super.transform(tree)
      }

    }

    private val intoConstructorTransformer = new IntoCtorTransformer

    // Move tree into constructor, take care of changing owner from `oldowner` to constructor symbol
    def intoConstructor(oldowner: Symbol, tree: Tree) =
      intoConstructorTransformer transform tree.changeOwner(oldowner -> constr.symbol)

    // Should tree be moved in front of super constructor call?
    def canBeMoved(tree: Tree) = tree match {
      case ValDef(mods, _, _, _) => (mods hasFlag PRESUPER | PARAMACCESSOR)
      case _                     => false
    }

    // Create an assignment to class field `to` with rhs `from`
    def mkAssign(to: Symbol, from: Tree): Assign = {
      val res = localTyper.typedPos(to.pos) { Assign(Select(This(clazz), to), from) }
      res.asInstanceOf[Assign]
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
        IF (from OBJ_EQ NULL) THEN Throw(gen.mkZero(ThrowableTpe)) ELSE result
      }
    }

    // The list of definitions that go into class
    val defBuf = new ListBuffer[Tree]

    // The auxiliary constructors, separate from the defBuf since they should
    // follow the primary constructor
    val auxConstructorBuf = new ListBuffer[Tree]

    // The list of statements that go into constructor after and including the superclass constructor call
    val constrStatBuf = new ListBuffer[Tree]

    // The list of early initializer statements that go into constructor before the superclass constructor call
    val constrPrefixBuf = new ListBuffer[Tree]

    // The early initialized field definitions of the class (these are the class members)
    val presupers = treeInfo.preSuperFields(stats)

    // generate code to copy pre-initialized fields
    for (stat <- constrBody.stats) {
      constrStatBuf += stat
      stat match {
        case ValDef(mods, name, _, _) if (mods hasFlag PRESUPER) =>
          // stat is the constructor-local definition of the field value
          val fields = presupers filter (_.getterName == name)
          assert(fields.length == 1)
          val to = fields.head.symbol
          if (!to.tpe.isInstanceOf[ConstantType])
            constrStatBuf += mkAssign(to, Ident(stat.symbol))
        case _ =>
      }
    }

    // Triage all template definitions to go into defBuf/auxConstructorBuf, constrStatBuf, or constrPrefixBuf.
    for (stat <- stats) stat match {
      case DefDef(_,_,_,_,_,rhs) =>
        // methods with constant result type get literals as their body
        // all methods except the primary constructor go into template
        stat.symbol.tpe match {
          case MethodType(List(), tp @ ConstantType(c)) =>
            defBuf += deriveDefDef(stat)(Literal(c) setPos _.pos setType tp)
          case _ =>
            if (stat.symbol.isPrimaryConstructor) ()
            else if (stat.symbol.isConstructor) auxConstructorBuf += stat
            else defBuf += stat
        }
      case ValDef(_, _, _, rhs) =>
        // val defs with constant right-hand sides are eliminated.
        // for all other val defs, an empty valdef goes into the template and
        // the initializer goes as an assignment into the constructor
        // if the val def is an early initialized or a parameter accessor, it goes
        // before the superclass constructor call, otherwise it goes after.
        // Lazy vals don't get the assignment in the constructor.
        if (!stat.symbol.tpe.isInstanceOf[ConstantType]) {
          if (rhs != EmptyTree && !stat.symbol.isLazy) {
            val rhs1 = intoConstructor(stat.symbol, rhs)
            (if (canBeMoved(stat)) constrPrefixBuf else constrStatBuf) += mkAssign(
              stat.symbol, rhs1)
          }
          defBuf += deriveValDef(stat)(_ => EmptyTree)
        }
      case ClassDef(_, _, _, _) =>
        // classes are treated recursively, and left in the template
        defBuf += new ConstructorTransformer(unit).transform(stat)
      case _ =>
        // all other statements go into the constructor
        constrStatBuf += intoConstructor(impl.symbol, stat)
    }

    // ----------------------------- by now triaging completed -----------------------------

    val JumpOverOutersInfo(cachedOuterFieldDecls, cachingFieldInits) = jumpOverRedundantOuters()

    defBuf ++= cachedOuterFieldDecls

    if (cachingFieldInits.nonEmpty) {
      debuglog(
        s"In class ${clazz.debugLocationString}, multi-step navigation over outer replaced: ${cachingFieldInits.mkString("; ")}"
      )
    }

    populateOmittables()

    // Initialize all parameters fields that must be kept.
    val paramInits = paramAccessors filter mustbeKept map { acc =>
      // Check for conflicting symbol amongst parents: see bug #1960.
      // It would be better to mangle the constructor parameter name since
      // it can only be used internally, but I think we need more robust name
      // mangling before we introduce more of it.
      val conflict = clazz.info.nonPrivateMember(acc.name) filter (s => s.isGetter && !s.isOuterField && s.enclClass.isTrait)
      if (conflict ne NoSymbol)
        unit.error(acc.pos, "parameter '%s' requires field but conflicts with %s".format(acc.name, conflict.fullLocationString))

      copyParam(acc, parameter(acc))
    }

    /* Return a pair consisting of (all statements up to and including superclass and trait constr calls, rest) */
    def splitAtSuper(stats: List[Tree]) = {
      def isConstr(tree: Tree) = (tree.symbol ne null) && tree.symbol.isConstructor
      val (pre, rest0) = stats span (!isConstr(_))
      val (supercalls, rest) = rest0 span (isConstr(_))
      (pre ::: supercalls, rest)
    }

    val (uptoSuperStats, remainingConstrStats0) = splitAtSuper(constrStatBuf.toList)
    var remainingConstrStats = remainingConstrStats0

    rewriteDelayedInit()

    val ctorStmsAfterParamInits = {
      constrPrefixBuf.toList ::: uptoSuperStats ::: guardSpecializedInitializer(remainingConstrStats)
    }

    // Assemble final constructor
    defBuf += deriveDefDef(constr)(_ =>
      treeCopy.Block(
        constrBody,
        paramInits ::: cachingFieldInits ::: ctorStmsAfterParamInits,
        constrBody.expr))

    // Followed by any auxiliary constructors
    defBuf ++= auxConstructorBuf

    // Unlink all fields that can be dropped from class scope
    for (sym <- clazz.info.decls ; if !mustbeKept(sym))
      clazz.info.decls unlink sym

    // Eliminate all field definitions that can be dropped from template
    val transformed: Template = deriveTemplate(impl)(_ => defBuf.toList filter (stat => mustbeKept(stat.symbol)))

  } // TemplateTransformer

  /*
   * Motivation
   * ----------
   *
   * This transformation
   *   - shortens navigation-paths over outer-instances to minimize heap retention at runtime,
   *   - by caching not the direct outer-instance but the nearest outer-instance that's actually needed.
   *
   * In particular, this transformation benefits actors, which usually hand out closures that outlive the actor.
   *
   *
   * Before and after this transformation
   * ------------------------------------
   *
   * For an inner class C fulfilling the preconditions detailed in `preconds()` (for example, an anonymous-closure),
   * it's possible to visit all usages of outer-accessors to find
   * the innermost outer-instance of C that's actually needed (there's only one, details in `jumpOverRedundantOuters()).
   * That's the outer-instance to be cached in place of the direct outer-instance.
   *
   * Before this transformation:
   *   - C holds its direct-outer in C's outer-field
   *
   * After this transformation:
   *   - C holds the nearest outer-instance that's actually needed in a field named "cachedOuter$"
   *   - accesses over outer-chains are simplified to take as starting point the cached outer-instance
   *   - the original outer-field isn't removed by this transformation but by `OmittablesHelper`
   *
   *
   * Interplay with other transforms, Performance impact
   * ---------------------------------------------------
   *
   * This transformation is self-contained (in the constructors phase) ie none of the other compilation phases
   * need perform additional work in support of `JumpOverOuters`.
   *
   * Irrespective of object size and lifetime, this transformation never degrades performance nor increases memory footprint,
   * thus it's always applied when conditions are fulfilled.
   *
   *
   * Overview of the implementation
   * ------------------------------
   *
   * Once triaging of template-members has been completed,
   * and before unused outer-fields are elided, it's `JumpOverOuters` time.
   *
   * The implementation is easier to grok from its entry point: `jumpOverRedundantOuters()`
   * Its description summarizes the utility methods at play (also documented)
   * and the role played by the `LinksCollector` utility class.
   *
   */
  private trait JumpOverOuters { self: TemplateTransformer =>

    // ----------- inspector methods -----------

    private def isOuterLoad(expr: Tree): Boolean = (
      expr.isInstanceOf[Apply]  && expr.symbol.isOuterAccessor ||
      expr.isInstanceOf[Select] && expr.symbol.isOuterField
    )

    private def isSelOnOuter(tree: Tree): Boolean = tree match {
      case sel: Select => isOuterLoad(sel.qualifier)
      case _           => false
    }

    // ----------- wrappers -----------

    /*
     * An `OuterLoad` demarcates an expression in rhs position that evaluates to an outer-instance.
     * An `OuterLoad` comes in two shapes:
     *   - Apply:  ie invocation of an outer-accessor
     *   - Select: of an outer-field
     * A third shape, Ident, can only occur in the body of a constructor, denoting *the* outer-value received via a param.
     * For the purposes of retaining less heap, there's no gain in shortening access-chains-over-outer-instances
     * in constructors, which aren't visted as part of this analysis. Therefore that third shape doesn't show up.
     */
    class OuterLoad(val expr: Tree) {

      def symbol = expr.symbol

      def isWellFormed = (isOuterLoad(expr) && isRootedAtThis)

      @scala.annotation.tailrec final def isRootedAtThis: Boolean = driller(expr) match {
        case NonValidOuterLoad => false
        case RootOuterLoad     => true
        case load              => load.isRootedAtThis
      }

      val driller: Tree => OuterLoad = {
        case Apply(Select(quali, _), Nil) if isOuterLoad(quali) => OuterLoad(quali)
        case s @ Select(t : This, _)      if t.symbol == clazz  => RootOuterLoad
        case Select(quali, _)             if isOuterLoad(quali) => OuterLoad(quali)
        case _                                                  => NonValidOuterLoad
      }

      def prevOuterLink: OuterLoad = driller(expr) match {
        case NonValidOuterLoad => abort(s"For the purposes of JumpOverOuters, an access-chain over outer-accessors isn't supposed to be like $expr")
        case load              => load
      }

      /*
       * The navigation path this OuterLoad builds upon. This OuterLoad isn't part of it.
       * In terms of the source code for an access-path with this OuterLoad as last link,
       * the first element returned would appears next-to-last.
       */
      def baseStack: List[OuterLoad] = prevOuterLink match {
        case RootOuterLoad => Nil
        case load          => load :: load.baseStack
      }

      override def equals(that: Any): Boolean = that match {
        case load: OuterLoad =>
          if (load eq this) true
          else if (this.expr == null || load.expr == null) {
            this eq load
          }
          else {
            (load.expr == this.expr)
          }
        case _ => false
      }

      override def hashCode() = (if (expr == null) 0 else expr.hashCode())

      override def toString = s"OuterLoad($expr)"

    }
    object OuterLoad { def apply(expr: Tree) = new OuterLoad(expr) }

    // represents a non-valid OuterLoad (theme: "avoid needless Options")
    object NonValidOuterLoad extends OuterLoad(null) { override def toString = "NonValidOuterLoad" }
    // represents the starting point of an access-chain over outer-references (ie obtained from This)
    object     RootOuterLoad extends OuterLoad(null) { override def toString = "RootOuterLoad" }

    /*
     * A `SelOnOuter` denotes an expression in rhs position that selects a member on an outer-instance.
     * For the purposes of this analyis, `SelOnOuter` are distinguished:
     *   - selecting an outer-instance (thus forming a chain of outer-accessors)
     *   - selecting anything else.
     */
    private case class SelOnOuter(sel: Select) {
      def symbol = sel.symbol
      def nestedOuterLoad: OuterLoad  = OuterLoad(sel.qualifier)
      def isPartOfOuterChain: Boolean = (sel.symbol.isOuterAccessor || sel.symbol.isOuterField)
    }

    // ----------- collecting individual links of outer-chains -----------

    class LinksCollector extends Traverser {

      private val selsOnOuter = mutable.Set.empty[SelOnOuter]
      private val outerLoads  = mutable.Set.empty[OuterLoad]

      // on purpose `paramInits` aren't visited to avoid trivial "shortest chains"
      // on purpose `constrStatBuf` not visited because *the* outer value is always available (as param-value) in the primary-ctor
      traverseTrees(defBuf.toList)
      traverseTrees(auxConstructorBuf.toList)

      private val outersPartOfNonOuterSelection = selsOnOuter filterNot (_.isPartOfOuterChain) map (_.nestedOuterLoad)

      /*
       * `initialLeaves`: Usages of outer-getters and outer-fields, with the property that
       *   an outer-instance thus obtained is in turn used:
       *     (a) to grab some non-outer member, or
       *     (b) the outer-instance is used as-is.
       * An OuterLoad in `leaves` appears as the last link in an access chain (aka "leaf").
       */
      val Pair(initialLeaves, inUseByOthers) = {
        val outersNotNavigatedFurther = {
          val isNavigatedFurther = (selsOnOuter map (_.nestedOuterLoad))
          outerLoads filterNot isNavigatedFurther
        }
        (outersPartOfNonOuterSelection ++ outersNotNavigatedFurther) partition (_.isRootedAtThis)
      }

      override def traverse(tree: Tree) {
        tree match {

          case dd: DefDef =>
            // don't count field-reads in outer-getters otherwise a trivial "shortest chain"
            if (!dd.symbol.isOuterAccessor) { super.traverse(dd) }

          case _ =>
            if (isSelOnOuter(tree)) {
              selsOnOuter += SelOnOuter(tree.asInstanceOf[Select])
            }
            else if (isOuterLoad(tree)) {
              outerLoads += OuterLoad(tree)
            }
            super.traverse(tree)

        }
      }

    }

    /*
     * Applicability conditions
     * ------------------------
     *
     * An inner-class C is a candidate for the JumpOverOuters transformation only if it fulfills all of:
     *   - final
     *         (thus we know all usage-sites of outer-references)
     *   - doesn't own nested classes itself
     *         (which simplifies the analysis, not an inherent limitation)
     *   - doesn't extend an inner-class
     *         (to avoid rare corner cases, details in `preconds()`)
     */
    private def preconds = {

      def hasNestedClass = (defBuf exists (_.isInstanceOf[ClassDef]))

      // `mustbeKept()` as of yet not well-defined because `populateOmittables()` runs after `jumpOverRedundantOuters()`
      // thus it might well happen `hasOuter == true` yet it's not in use.
      // No problem: JumpOverOuters can deal with that (it changes nothing in that case).
      def hasOuter = (clazz.info.decls exists (_.isOuterField))

      /*
       * To simplify the analysis of outer-chains, we focus on the (by far most common ) case where no outer-field is inherited.
       * For example, class N below is not eligible for this transformation because it extends an inner-class:
       *
       *     class O {
       *       class I1
       *       class I2 {
       *         class N extends I1
       *       }
       *     }
       *
       * In detail, N declares an outer field in addition to the outer-field inherited from I1
       * (I1 and N have different owners, and the inherited outer-field has protected access-level)
       *
       */
      def extendsInnerClass = (clazz.ancestors exists explicitOuter.isInner)


      (!isDelayedInitSubclass && clazz.isEffectivelyFinal && !hasNestedClass && hasOuter && !extendsInnerClass)
    }

    /*
     * Entry point for the JumpOverOuters transformation
     * -------------------------------------------------
     *
     * Rewrites template-members (other than the primary constructor)
     * to shorten access-chains over outer-instances. This involves several steps:
     *
     *   (1) checking preconditions
     *
     *   (2) collecting outer-chains via `LinksCollector`
     *
     *   (3) inspecting outer-chains to find the longest outer-chains that aren't navigated further,
     *       either because a non-outer member is selected on the last link of the chain;
     *       or because the outer-instance given by the last link is used as-is.
     *
     *   (4) in case the thus found "nearmost outer-instance that's actually needed"
     *       is different from the direct-outer (ie, it lies beyond the direct-outer)
     *         - a field is added to cache it,
     *         - an assignment statement to initialize the newly added field will be added to the primary constructor,
     *         - access-chains are rewritten
     *
     */
    def jumpOverRedundantOuters(): JumpOverOutersInfo = {

      if (!preconds) { return NoJumpOverOutersInfo }

      val linksCollector = new LinksCollector

      if (linksCollector.inUseByOthers.nonEmpty) {
        // sample snippet to get here:
        //   if (x1.$isInstanceOf[reflect.internal.Trees#Tree]().&&((x1.$asInstanceOf[reflect.internal.Trees#Tree]()).$outer().eq($anonfun$1.this.$outer.global())))
        return NoJumpOverOutersInfo
      }

      val leaves = mutable.Set.empty[OuterLoad] ++ linksCollector.initialLeaves

      /*
       * Symbols for members (fields, getters) that provide an outer-instance,
       * such that those members appear in at least one access-chain.
       *
       * Initially this set contains candidates, which are removed as other already-cached outer-instances
       * are discovered that can be used to access the removed candidate.
       */
      val isCached = mutable.Set.empty[Symbol] ++ (leaves map (_.symbol))

      def notCached(load: OuterLoad) = !isCached(load.symbol)

      def dontCache(load: OuterLoad) { isCached -= load.symbol }

      /*
       * Caching the outer-instance given by an OuterLoad `load` makes sense
       * only if it never appears in an access chain after another outer-instance `prev` already being cached.
       * For in that case we're not making GC any easier, and we could navigate our way from `prev` to `load`.
       *
       * Starting with outer-chains as gathered by `LinksCollector` (called `leaves` below)
       * they are progressively shortened (following the strategy described above).
       * What remains is a bunch of access-chains all ending at the "nearest outer-instance that's actually needed".
       */
      var shrinked = false
      do {
        shrinked = false
        val snapshot = leaves.toList
        for(leaf <- snapshot) {
          // let's see whether any "nearer" outer-instance is being cached, to remove as caching-candidates those reachable from it.
          val prefix = leaf.baseStack.reverse
          val fstRetainedAndTheRest = (prefix dropWhile notCached)
          fstRetainedAndTheRest match {
            case nearestMustRetain :: reachableFromNearest =>
              shrinked    = true
              leaves     -= leaf
              leaves     += nearestMustRetain
              (leaf :: reachableFromNearest) foreach dontCache
            case _ =>
              ()
          }
        }
      } while (shrinked)

      /*
       * Not all "leaf" expressions should be shortened. For example, `this.$outer` is already in "cached-form".
       * The `longForms` picked below are those leaf expressions consisting of at least an additional navigation step.
       * Each such longForm will be replaced by a read of the newly added field (ie, the cache).
       */
      val longForms: collection.Set[Tree] =
        for(
          leaf <- leaves;
          steppingStones = leaf.baseStack.reverse filterNot (_ == RootOuterLoad);
          if steppingStones.nonEmpty
        ) yield leaf.expr

      if (longForms.isEmpty) { return NoJumpOverOutersInfo }

      /*
       * More than one longForm may denote the same outer-instance. A single field should cache it.
       */
      val cachingField = mutable.Map.empty[/*Outer*/ Symbol, /*Field*/ Symbol]

      /*
       * After `paramInits` goes the initialization of fields caching outer-instances (ie in the primary constructor).
       */
      var cachingFieldInits: List[Assign] = Nil

      for(longForm <- longForms; osym = longForm.symbol; if !cachingField.isDefinedAt(osym)) {

        val fieldName = unit.freshTermName("cachedOuter$")
        val fsym = clazz.newValue(fieldName, NoPosition, SYNTHETIC | FINAL | PRIVATE)
        fsym.setInfoAndEnter(osym.tpe.resultType)

        cachingField += (osym -> fsym)

        cachingFieldInits ::= {
          // no need to emit `if ($outer.eq(null)) throw null ...`
          // because the same effect will occur upon initialization of the cached non-direct outer-instance
          // for example: $anonfun$xyz.this.cachedOuter$1 = $outer.$outer();
          val initTree = mkAssign(fsym, intoConstructor(oldowner = null, tree = longForm))
          resetPos traverse initTree
          initTree
        }
      }

      val cachedOuterFieldDecls: List[ValDef] =
        for (fsym <- cachingField.values.toList)
        yield {
          (localTyper typed ValDef(fsym)).asInstanceOf[ValDef]
        }

      object longToShortForm extends Transformer {
        private def getfield(expr: Tree): Select = {
          val osym = expr.symbol
          val fsym = cachingField(osym)
          val sel  = localTyper.typedPos(expr.pos) { Select(This(clazz), fsym) }
          sel.asInstanceOf[Select]
        }
        override def transform(tree: Tree): Tree = tree match {
          case sel: Select if longForms(sel) => getfield(sel)
          case app: Apply  if longForms(app) => getfield(app)
          case other                         => super.transform(other)
        }
      }

      val inDef  = defBuf.toList
      val inAux  = auxConstructorBuf.toList
      val outDef = (longToShortForm transformTrees inDef)
      val outAux = (longToShortForm transformTrees inAux)
      if (inDef ne outDef) {
        defBuf.clear(); defBuf ++= outDef
      }
      if (inAux ne outAux) {
        auxConstructorBuf.clear(); auxConstructorBuf ++= outAux
      }

      JumpOverOutersInfo(cachedOuterFieldDecls, cachingFieldInits)

    } // method jumpOverRedundantOuters()

    case class JumpOverOutersInfo(cachedOuterFieldDecls: List[ValDef], cachingFieldInits: List[Assign])

    val NoJumpOverOutersInfo = JumpOverOutersInfo(Nil, Nil)

  } // JumpOverOuters

}
