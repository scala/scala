
/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala
package tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import mutable.ListBuffer
import symtab.Flags._

/** This phase performs the following functions, each of which could be split out in a
 *  mini-phase:
 *
 *  (1) Adds super accessors for all super calls that either
 *  appear in a trait or have as a target a member of some outer class.
 *
 *  (2) Converts references to parameter fields that have the same name as a corresponding
 *  public parameter field in a superclass to a reference to the superclass
 *  field (corresponding = super class field is initialized with subclass field).
 *  This info is pre-computed by the `alias` field in Typer. `dotc` follows a different
 *  route; it computes everything in SuperAccessors and changes the subclass field
 *  to a forwarder instead of manipulating references. This is more modular.
 *
 *  (3) Adds protected accessors if the access to the protected member happens
 *  in a class which is not a subclass of the member's owner.
 *
 *  (4) Mangles the names of class-members which are
 *  private up to an enclosing non-package class, in order to avoid overriding conflicts.
 *  This is a dubious, and it would be better to deprecate class-qualified privates.
 *
 *  (5) This phase also sets SPECIALIZED flag on type parameters with
 *  `@specialized` annotation. We put this logic here because the
 *  flag must be set before pickling.
 *
 *  It also checks that:
 *
 *  (1) Symbols accessed from super are not abstract, or are overridden by
 *  an abstract override.
 *
 *  (2) If a symbol accessed accessed from super is defined in a real class (not a trait),
 *  there are no abstract members which override this member in Java's rules
 *  (see SI-4989; such an access would lead to illegal bytecode)
 *
 *  (3) Super calls do not go to some synthetic members of Any (see isDisallowed)
 *
 *  (4) Super calls do not go to synthetic field accessors
 *
 *  (5) A class and its companion object do not both define a class or module with the
 *  same name.
 *
 *  TODO: Rename phase to "Accessors" because it handles more than just super accessors
 */
abstract class SuperAccessors extends transform.Transform with transform.TypingTransformers {
  import global._
  import definitions._
  import analyzer.{ restrictionError }

  /** the following two members override abstract members in Transform */
  val phaseName: String = "superaccessors"

  /** The following flags may be set by this phase: */
  override def phaseNewFlags: Long = notPRIVATE

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new SuperAccTransformer(unit)

  class SuperAccTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    /** validCurrentOwner arrives undocumented, but I reverse engineer it to be
     *  a flag for needsProtectedAccessor which is false while transforming either
     *  a by-name argument block or a closure.  This excludes them from being
     *  considered able to access protected members via subclassing (why?) which in turn
     *  increases the frequency with which needsProtectedAccessor will be true.
     */
    private var validCurrentOwner = true
    private val accDefs = mutable.Map[Symbol, ListBuffer[Tree]]()

    private def storeAccessorDefinition(clazz: Symbol, tree: Tree) = {
      val buf = accDefs.getOrElse(clazz, sys.error("no acc def buf for "+clazz))
      buf += typers(clazz) typed tree
    }
    private def ensureAccessor(sel: Select, mixName: TermName = nme.EMPTY) = {
      val Select(qual, name) = sel
      val sym                = sel.symbol
      val clazz              = qual.symbol
      val supername          = nme.superName(name, mixName)
      val superAcc = clazz.info.decl(supername).suchThat(_.alias == sym) orElse {
        debuglog(s"add super acc ${sym.fullLocationString} to $clazz")
        val acc = clazz.newMethod(supername, sel.pos, SUPERACCESSOR | PRIVATE | ARTIFACT) setAlias sym
        val tpe = clazz.thisType memberType sym match {
          case t if sym.isModuleNotMethod => NullaryMethodType(t)
          case t                          => t
        }
        acc setInfoAndEnter (tpe cloneInfo acc)
        // Diagnostic for SI-7091
        if (!accDefs.contains(clazz))
          reporter.error(sel.pos, s"Internal error: unable to store accessor definition in ${clazz}. clazz.hasPackageFlag=${clazz.hasPackageFlag}. Accessor required for ${sel} (${showRaw(sel)})")
        else storeAccessorDefinition(clazz, DefDef(acc, EmptyTree))
        acc
      }

      atPos(sel.pos)(Select(gen.mkAttributedThis(clazz), superAcc) setType sel.tpe)
    }

    private def transformArgs(params: List[Symbol], args: List[Tree]) = {
      treeInfo.mapMethodParamsAndArgs(params, args) { (param, arg) =>
        if (isByNameParamType(param.tpe))
          withInvalidOwner(transform(arg))
        else transform(arg)
      }
    }

    /** Check that a class and its companion object to not both define
     *  a class or module with same name
     */
    private def checkCompanionNameClashes(sym: Symbol) =
      if (!sym.owner.isModuleClass) {
        val linked = sym.owner.linkedClassOfClass
        if (linked != NoSymbol) {
          var other = linked.info.decl(sym.name.toTypeName).filter(_.isClass)
          if (other == NoSymbol)
            other = linked.info.decl(sym.name.toTermName).filter(_.isModule)
          if (other != NoSymbol)
            reporter.error(sym.pos, "name clash: "+sym.owner+" defines "+sym+
                       "\nand its companion "+sym.owner.companionModule+" also defines "+
                       other)
        }
      }

    private def transformSuperSelect(sel: Select): Tree = {
      val Select(sup @ Super(_, mix), name) = sel
      val sym   = sel.symbol
      val clazz = sup.symbol

      if (sym.isDeferred) {
        val member = sym.overridingSymbol(clazz)
        if (mix != tpnme.EMPTY || member == NoSymbol ||
            !(member.isAbstractOverride && member.isIncompleteIn(clazz)))
          reporter.error(sel.pos, ""+sym.fullLocationString+" is accessed from super. It may not be abstract "+
                               "unless it is overridden by a member declared `abstract' and `override'")
      } else if (mix == tpnme.EMPTY && !sym.owner.isTrait){
        // SI-4989 Check if an intermediate class between `clazz` and `sym.owner` redeclares the method as abstract.
        val intermediateClasses = clazz.info.baseClasses.tail.takeWhile(_ != sym.owner)
        intermediateClasses.map(sym.overridingSymbol).find(s => s.isDeferred && !s.isAbstractOverride && !s.owner.isTrait).foreach {
          absSym =>
            reporter.error(sel.pos, s"${sym.fullLocationString} cannot be directly accessed from ${clazz} because ${absSym.owner} redeclares it as abstract")
        }
      }

      def mixIsTrait = sup.tpe match {
        case SuperType(thisTpe, superTpe) => superTpe.typeSymbol.isTrait
      }

      val needAccessor = name.isTermName && {
        mix.isEmpty && (clazz.isTrait || clazz != currentClass || !validCurrentOwner) ||
        // SI-8803. If we access super[A] from an inner class (!= currentClass) or closure (validCurrentOwner),
        // where A is the superclass we need an accessor. If A is a parent trait we don't: in this case mixin
        // will re-route the super call directly to the impl class (it's statically known).
        !mix.isEmpty && (clazz != currentClass || !validCurrentOwner) && !mixIsTrait
      }

      if (needAccessor)
        ensureAccessor(sel, mix.toTermName)
      else sel
    }

    // Disallow some super.XX calls targeting Any methods which would
    // otherwise lead to either a compiler crash or runtime failure.
    private lazy val isDisallowed = {
      import definitions._
      Set[Symbol](Any_isInstanceOf, Object_isInstanceOf, Any_asInstanceOf, Object_asInstanceOf, Object_==, Object_!=, Object_##)
    }

    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol

      def mayNeedProtectedAccessor(sel: Select, args: List[Tree], goToSuper: Boolean) =
        if (needsProtectedAccessor(sym, tree.pos)) {
          debuglog("Adding protected accessor for " + tree)

          transform(makeAccessor(sel, args))
        }
        else if (goToSuper) super.transform(tree)
        else tree

      try tree match {
        // Don't transform patterns or strange trees will reach the matcher (ticket #4062)
        case CaseDef(pat, guard, body) =>
          treeCopy.CaseDef(tree, pat, transform(guard), transform(body))

        case ClassDef(_, _, _, _) =>
          def transformClassDef = {
          checkCompanionNameClashes(sym)
          val decls = sym.info.decls
          for (s <- decls) {
            if (s.privateWithin.isClass && !s.isProtected && !s.privateWithin.isModuleClass &&
                !s.hasFlag(EXPANDEDNAME) && !s.isConstructor) {
              val savedName = s.name
              decls.unlink(s)
              s.expandName(s.privateWithin)
              decls.enter(s)
              log("Expanded '%s' to '%s' in %s".format(savedName, s.name, sym))
            }
          }
          super.transform(tree)
          }
          transformClassDef

        case ModuleDef(_, _, _) =>
          checkCompanionNameClashes(sym)
          super.transform(tree)

        case Template(_, _, body) =>
          def transformTemplate = {
          val ownAccDefs = new ListBuffer[Tree]
          accDefs(currentOwner) = ownAccDefs

          // ugly hack... normally, the following line should not be
          // necessary, the 'super' method taking care of that. but because
          // that one is iterating through parents (and we dont want that here)
          // we need to inline it.
          curTree = tree
          val body1 = atOwner(currentOwner)(transformTrees(body))
          accDefs -= currentOwner
          ownAccDefs ++= body1
          deriveTemplate(tree)(_ => ownAccDefs.toList)
          }
          transformTemplate

        case TypeApply(sel @ Select(This(_), name), args) =>
          mayNeedProtectedAccessor(sel, args, goToSuper = false)

        // set a flag for all type parameters with `@specialized` annotation so it can be pickled
        case typeDef: TypeDef if typeDef.symbol.deSkolemize.hasAnnotation(definitions.SpecializedClass) =>
          debuglog("setting SPECIALIZED flag on typeDef.symbol.deSkolemize where typeDef = " + typeDef)
          // we need to deSkolemize symbol so we get the same symbol as others would get when
          // inspecting type parameter from "outside"; see the discussion of skolems here:
          // https://groups.google.com/d/topic/scala-internals/0j8laVNTQsI/discussion
          typeDef.symbol.deSkolemize.setFlag(SPECIALIZED)
          typeDef

        case sel @ Select(qual, name) =>
          def transformSelect = {

          // FIXME Once Inliners is modified with the "'meta-knowledge' that all fields accessed by @inline will be made public" [1]
          //       this can be removed; the correct place for this in in ExplicitOuter.
          //
          // [1] https://groups.google.com/forum/#!topic/scala-internals/iPkMCygzws4
          //
          if (closestEnclMethod(currentOwner) hasAnnotation definitions.ScalaInlineClass)
            sym.makeNotPrivate(sym.owner)

          qual match {
            case This(_) =>
              // warn if they are selecting a private[this] member which
              // also exists in a superclass, because they may be surprised
              // to find out that a constructor parameter will shadow a
              // field. See SI-4762.
              if (settings.warnPrivateShadow) {
                if (sym.isPrivateLocal && sym.paramss.isEmpty) {
                  qual.symbol.ancestors foreach { parent =>
                    parent.info.decls filterNot (x => x.isPrivate || x.isLocalToThis) foreach { m2 =>
                      if (sym.name == m2.name && m2.isGetter && m2.accessed.isMutable) {
                        reporter.warning(sel.pos,
                          sym.accessString + " " + sym.fullLocationString + " shadows mutable " + m2.name
                            + " inherited from " + m2.owner + ".  Changes to " + m2.name + " will not be visible within "
                            + sym.owner + " - you may want to give them distinct names.")
                      }
                    }
                  }
                }
              }


              def isAccessibleFromSuper(sym: Symbol) = {
                val pre = SuperType(sym.owner.tpe, qual.tpe)
                localTyper.context.isAccessible(sym, pre, superAccess = true)
              }

              // Direct calls to aliases of param accessors to the superclass in order to avoid
              // duplicating fields.
              // ... but, only if accessible (SI-6793)
              if (sym.isParamAccessor && sym.alias != NoSymbol && isAccessibleFromSuper(sym.alias)) {
                val result = (localTyper.typedPos(tree.pos) {
                  Select(Super(qual, tpnme.EMPTY) setPos qual.pos, sym.alias)
                }).asInstanceOf[Select]
                debuglog("alias replacement: " + tree + " ==> " + result); //debug
                localTyper.typed(gen.maybeMkAsInstanceOf(transformSuperSelect(result), sym.tpe, sym.alias.tpe, beforeRefChecks = true))
              } else {
                /*
                 * A trait which extends a class and accesses a protected member
                 *  of that class cannot implement the necessary accessor method
                 *  because its implementation is in an implementation class (e.g.
                 *  Foo$class) which inherits nothing, and jvm access restrictions
                 *  require the call site to be in an actual subclass. So non-trait
                 *  classes inspect their ancestors for any such situations and
                 *  generate the accessors.  See SI-2296.
                 */
                // FIXME - this should be unified with needsProtectedAccessor, but some
                // subtlety which presently eludes me is foiling my attempts.
                val shouldEnsureAccessor = (
                     currentClass.isTrait
                  && sym.isProtected
                  && sym.enclClass != currentClass
                  && !sym.owner.isPackageClass // SI-7091 no accessor needed package owned (ie, top level) symbols
                  && !sym.owner.isTrait
                  && sym.owner.enclosingPackageClass != currentClass.enclosingPackageClass
                  && qual.symbol.info.member(sym.name).exists
                  && !needsProtectedAccessor(sym, tree.pos)
                )
                if (shouldEnsureAccessor) {
                  log("Ensuring accessor for call to protected " + sym.fullLocationString + " from " + currentClass)
                  ensureAccessor(sel)
                }
                else
                  mayNeedProtectedAccessor(sel, EmptyTree.asList, goToSuper = false)
              }

            case Super(_, mix) =>
              if (sym.isValue && !sym.isMethod || sym.hasAccessorFlag) {
                if (!settings.overrideVars)
                  reporter.error(tree.pos, "super may not be used on " + sym.accessedOrSelf)
              } else if (isDisallowed(sym)) {
                reporter.error(tree.pos, "super not allowed here: use this." + name.decode + " instead")
              }
              transformSuperSelect(sel)

            case _ =>
              mayNeedProtectedAccessor(sel, EmptyTree.asList, goToSuper = true)
          }
          }
          transformSelect

        case DefDef(_, _, _, _, _, _) if tree.symbol.isMethodWithExtension =>
          deriveDefDef(tree)(rhs => withInvalidOwner(transform(rhs)))

        case TypeApply(sel @ Select(qual, name), args) =>
          mayNeedProtectedAccessor(sel, args, goToSuper = true)

        case Assign(lhs @ Select(qual, name), rhs) =>
          def transformAssign = {
          if (lhs.symbol.isVariable &&
              lhs.symbol.isJavaDefined &&
              needsProtectedAccessor(lhs.symbol, tree.pos)) {
            debuglog("Adding protected setter for " + tree)
            val setter = makeSetter(lhs)
            debuglog("Replaced " + tree + " with " + setter)
            transform(localTyper.typed(Apply(setter, List(qual, rhs))))
          } else
            super.transform(tree)
          }
          transformAssign

        case Apply(fn, args) =>
          assert(fn.tpe != null, tree)
          treeCopy.Apply(tree, transform(fn), transformArgs(fn.tpe.params, args))

        case Function(vparams, body) =>
          withInvalidOwner {
            treeCopy.Function(tree, vparams, transform(body))
          }

        case _ =>
          super.transform(tree)
      }
      catch {
        case ex : AssertionError =>
          if (sym != null && sym != NoSymbol)
            Console.println("TRANSFORM: " + tree.symbol.sourceFile)

          Console.println("TREE: " + tree)
          throw ex
      }
    }

    /** a typer for each enclosing class */
    private var typers = immutable.Map[Symbol, analyzer.Typer]()

    /** Specialized here for performance; the previous blanked
     *  introduction of typers in TypingTransformer caused a >5%
     *  performance hit for the compiler as a whole.
     */
    override def atOwner[A](tree: Tree, owner: Symbol)(trans: => A): A = {
      val savedValid = validCurrentOwner
      if (owner.isClass) validCurrentOwner = true
      val savedLocalTyper = localTyper
      localTyper = localTyper.atOwner(tree, if (owner.isModule) owner.moduleClass else owner)
      typers = typers updated (owner, localTyper)
      val result = super.atOwner(tree, owner)(trans)
      localTyper = savedLocalTyper
      validCurrentOwner = savedValid
      typers -= owner
      result
    }

    private def withInvalidOwner[A](trans: => A): A = {
      val saved = validCurrentOwner
      validCurrentOwner = false
      try trans
      finally validCurrentOwner = saved
    }

    /** Add a protected accessor, if needed, and return a tree that calls
     *  the accessor and returns the same member. The result is already
     *  typed.
     */
    private def makeAccessor(tree: Select, targs: List[Tree]): Tree = {
      val Select(qual, _) = tree
      val sym = tree.symbol
      val clazz = hostForAccessorOf(sym, currentClass)

      assert(clazz != NoSymbol, sym)
      debuglog("Decided for host class: " + clazz)

      val accName    = nme.protName(sym.unexpandedName)
      val hasArgs    = sym.tpe.paramSectionCount > 0
      val memberType = refChecks.toScalaRepeatedParam(sym.tpe) // fix for #2413

      // if the result type depends on the this type of an enclosing class, the accessor
      // has to take an object of exactly this type, otherwise it's more general
      val objType = if (isThisType(memberType.finalResultType)) clazz.thisType else clazz.typeOfThis
      val accType = (protAcc: Symbol) => memberType match {
        case PolyType(tparams, restpe) =>
          // luc: question to author: should the tparams symbols not be cloned and get a new owner (protAcc)?
          PolyType(tparams, MethodType(List(protAcc.newSyntheticValueParam(objType)),
                                       restpe.cloneInfo(protAcc).asSeenFrom(qual.tpe, sym.owner)))
        case _ =>
          MethodType(List(protAcc.newSyntheticValueParam(objType)),
                     memberType.cloneInfo(protAcc).asSeenFrom(qual.tpe, sym.owner))
      }

      val protAcc = clazz.info.decl(accName).suchThat(s => s == NoSymbol || s.tpe =:= accType(s)) orElse {
        val newAcc = clazz.newMethod(nme.protName(sym.unexpandedName), tree.pos, newFlags = ARTIFACT)
        newAcc setInfoAndEnter accType(newAcc)

        val code = DefDef(newAcc, {
          val (receiver :: _) :: tail = newAcc.paramss
          val base: Tree              = Select(Ident(receiver), sym)
          val allParamTypes           = mapParamss(sym)(_.tpe)
          val args = map2(tail, allParamTypes)((params, tpes) => map2(params, tpes)(makeArg(_, receiver, _)))
          args.foldLeft(base)(Apply(_, _))
        })

        debuglog("created protected accessor: " + code)
        storeAccessorDefinition(clazz, code)
        newAcc
      }
      val selection = Select(This(clazz), protAcc)
      def mkApply(fn: Tree) = Apply(fn, qual :: Nil)
      val res = atPos(tree.pos) {
        targs.head match {
          case EmptyTree  => mkApply(selection)
          case _          => mkApply(TypeApply(selection, targs))
        }
      }
      debuglog(s"Replaced $tree with $res")
      if (hasArgs) localTyper.typedOperator(res) else localTyper.typed(res)
    }

    /** Adapt the given argument in call to protected member.
     *  Adaptation may add a cast to a path-dependent type, for instance
     *
     *  def prot$m(obj: Outer)(x: Inner) = obj.m(x.asInstanceOf[obj.Inner]).
     *
     *  such a cast might be necessary when m expects an Outer.this.Inner (the
     *  outer of 'obj' and 'x' have to be the same). This restriction can't be
     *  expressed in the type system (but is implicit when defining method m).
     *
     *  Also, it calls using repeated parameters are ascribed with ': _*'
     */
    private def makeArg(v: Symbol, obj: Symbol, pt: Type): Tree = {
      // owner class
      val clazz = pt match {
        case TypeRef(pre, _, _) => thisTypeOfPath(pre)
        case _                  => NoSymbol
      }
      val result = gen.paramToArg(v)
      if (clazz != NoSymbol && (obj.tpe.typeSymbol isSubClass clazz)) // path-dependent type
        gen.mkAsInstanceOf(result, pt.asSeenFrom(singleType(NoPrefix, obj), clazz))
      else
        result
    }

    /** Add an accessor for field, if needed, and return a selection tree for it .
     *  The result is not typed.
     */
    private def makeSetter(tree: Select): Tree = {
      val field = tree.symbol
      val clazz = hostForAccessorOf(field, currentClass)
      assert(clazz != NoSymbol, field)
      debuglog("Decided for host class: " + clazz)

      val accName = nme.protSetterName(field.unexpandedName)
      val protectedAccessor = clazz.info decl accName orElse {
        val protAcc      = clazz.newMethod(accName, field.pos, newFlags = ARTIFACT)
        val paramTypes   = List(clazz.typeOfThis, field.tpe)
        val params       = protAcc newSyntheticValueParams paramTypes
        val accessorType = MethodType(params, UnitTpe)

        protAcc setInfoAndEnter accessorType
        val obj :: value :: Nil = params
        storeAccessorDefinition(clazz, DefDef(protAcc, Assign(Select(Ident(obj), field.name), Ident(value))))

        protAcc
      }
      atPos(tree.pos)(Select(This(clazz), protectedAccessor))
    }

    /** Does `sym` need an accessor when accessed from `currentClass`?
     *  A special case arises for classes with explicit self-types. If the
     *  self type is a Java class, and a protected accessor is needed, we issue
     *  an error. If the self type is a Scala class, we don't add an accessor.
     *  An accessor is not needed if the access boundary is larger than the
     *  enclosing package, since that translates to 'public' on the host sys.
     *  (as Java has no real package nesting).
     *
     * If the access happens inside a 'trait', access is more problematic since
     * the implementation code is moved to an '$class' class which does not
     * inherit anything. Since we can't (yet) add accessors for 'required'
     * classes, this has to be signaled as error.
     */
    private def needsProtectedAccessor(sym: Symbol, pos: Position): Boolean = {
      val clazz = currentClass
      def accessibleThroughSubclassing =
        validCurrentOwner && clazz.thisSym.isSubClass(sym.owner) && !clazz.isTrait

      val isCandidate = (
           sym.isProtected
        && sym.isJavaDefined
        && !sym.isDefinedInPackage
        && !accessibleThroughSubclassing
        && (sym.enclosingPackageClass != currentClass.enclosingPackageClass)
        && (sym.enclosingPackageClass == sym.accessBoundary(sym.enclosingPackageClass))
      )
      val host = hostForAccessorOf(sym, clazz)
      def isSelfType = !(host.tpe <:< host.typeOfThis) && {
        if (host.typeOfThis.typeSymbol.isJavaDefined)
          restrictionError(pos, unit,
            "%s accesses protected %s from self type %s.".format(clazz, sym, host.typeOfThis)
          )
        true
      }
      def isJavaProtected = host.isTrait && sym.isJavaDefined && {
        restrictionError(pos, unit,
          sm"""$clazz accesses protected $sym inside a concrete trait method.
              |Add an accessor in a class extending ${sym.enclClass} as a workaround."""
        )
        true
      }
      isCandidate && !host.isPackageClass && !isSelfType && !isJavaProtected
    }

    /** Return the innermost enclosing class C of referencingClass for which either
     *  of the following holds:
     *     - C is a subclass of sym.owner or
     *     - C is declared in the same package as sym's owner
     */
    private def hostForAccessorOf(sym: Symbol, referencingClass: Symbol): Symbol = {
      if (referencingClass.isSubClass(sym.owner.enclClass)
          || referencingClass.thisSym.isSubClass(sym.owner.enclClass)
          || referencingClass.enclosingPackageClass == sym.owner.enclosingPackageClass) {
        assert(referencingClass.isClass, referencingClass)
        referencingClass
      } else if(referencingClass.owner.enclClass != NoSymbol)
        hostForAccessorOf(sym, referencingClass.owner.enclClass)
      else referencingClass
    }

    /** For a path-dependent type, return the this type. */
    private def thisTypeOfPath(path: Type): Symbol = path match {
      case ThisType(outerSym)  => outerSym
      case SingleType(rest, _) => thisTypeOfPath(rest)
      case _                   => NoSymbol
    }

    /** Is 'tpe' the type of a member of an enclosing class? */
    private def isThisType(tpe: Type): Boolean = tpe match {
      case ThisType(sym)           => sym.isClass && !sym.isPackageClass
      case TypeRef(pre, _, _)      => isThisType(pre)
      case SingleType(pre, _)      => isThisType(pre)
      case RefinedType(parents, _) => parents exists isThisType
      case AnnotatedType(_, tp)    => isThisType(tp)
      case _                       => false
    }
  }
}
