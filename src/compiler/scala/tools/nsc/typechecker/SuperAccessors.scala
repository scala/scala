/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable.ListBuffer
import symtab.Flags._

/** This phase adds super accessors for all super calls that
 *  either appear in a trait or have as a target a member of some outer class.
 *  It also replaces references to parameter accessors with aliases by super
 *  references to these aliases.
 *  The phase also checks that symbols accessed from super are not abstract,
 *  or are overridden by an abstract override.
 *  Finally, the phase also mangles the names of class-members which are private
 *  up to an enclosing non-package class, in order to avoid overriding conflicts.
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class SuperAccessors extends transform.Transform with transform.TypingTransformers {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._
  import definitions.{
    IntClass, UnitClass, ByNameParamClass, Any_asInstanceOf,
    Any_isInstanceOf, Object_isInstanceOf, Object_##, Object_==, Object_!=
  }

  /** the following two members override abstract members in Transform */
  val phaseName: String = "superaccessors"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new SuperAccTransformer(unit)

  class SuperAccTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    private var validCurrentOwner = true
    private var accDefs: List[(Symbol, ListBuffer[Tree])] = List()

    private def accDefBuf(clazz: Symbol) = accDefs find (_._1 == clazz) match {
      case Some((_, buf)) => buf
      case None => throw new AssertionError("no acc def buf for "+clazz)
    }

    private def transformArgs(args: List[Tree], params: List[Symbol]) =
      ((args, params).zipped map { (arg, param) =>
        if (param.tpe.typeSymbol == ByNameParamClass)
          withInvalidOwner { checkPackedConforms(transform(arg), param.tpe.typeArgs.head) }
        else transform(arg)
      }) :::
      (args drop params.length map transform)

    private def checkPackedConforms(tree: Tree, pt: Type): Tree = {
      if (tree.tpe exists (_.typeSymbol.isExistentialSkolem)) {
        val packed = localTyper.packedType(tree, NoSymbol)
        if (!(packed <:< pt)) {
          val errorContext = localTyper.context.make(localTyper.context.tree)
          errorContext.reportGeneralErrors = true
          analyzer.newTyper(errorContext).infer.typeError(tree.pos, packed, pt)
        }
      }
      tree
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
            unit.error(sym.pos, "name clash: "+sym.owner+" defines "+sym+
                       "\nand its companion "+sym.owner.companionModule+" also defines "+
                       other)
        }
      }

    private def transformSuperSelect(tree: Tree): Tree = tree match {
      case Select(sup @ Super(_, mix), name)  =>
        val sym = tree.symbol
        val clazz = sup.symbol

        if (sym.isDeferred) {
          val member = sym.overridingSymbol(clazz);
          if (mix != nme.EMPTY.toTypeName || member == NoSymbol ||
              !((member hasFlag ABSOVERRIDE) && member.isIncompleteIn(clazz)))
            unit.error(tree.pos, ""+sym+sym.locationString+" is accessed from super. It may not be abstract "+
                                 "unless it is overridden by a member declared `abstract' and `override'");
        }
        if (tree.isTerm && mix == nme.EMPTY.toTypeName &&
            (clazz.isTrait || clazz != currentOwner.enclClass || !validCurrentOwner)) {
          val supername = nme.superName(sym.name)
          var superAcc = clazz.info.decl(supername).suchThat(_.alias == sym)
          if (superAcc == NoSymbol) {
            if (settings.debug.value) log("add super acc " + sym + sym.locationString + " to `" + clazz);//debug
            superAcc =
              clazz.newMethod(tree.pos, supername)
                .setFlag(SUPERACCESSOR | PRIVATE)
                .setAlias(sym)
            var superAccTpe = clazz.thisType.memberType(sym)
            if (sym.isModule && !sym.isMethod) {
              // the super accessor always needs to be a method. See #231
              superAccTpe = PolyType(List(), superAccTpe)
            }
            superAcc.setInfo(superAccTpe.cloneInfo(superAcc))
            //println("creating super acc "+superAcc+":"+superAcc.tpe)//DEBUG
            clazz.info.decls enter superAcc;
            accDefBuf(clazz) += typers(clazz).typed(DefDef(superAcc, EmptyTree))
          }
          atPos(sup.pos) {
            Select(gen.mkAttributedThis(clazz), superAcc) setType tree.tpe;
          }
        } else {
          tree
        }
      case _ =>
        assert(tree.tpe.isError, tree)
        tree
    }

    // Disallow some super.XX calls targeting Any methods which would
    // otherwise lead to either a compiler crash or runtime failure.
    private def isDisallowed(sym: Symbol) = (
      (sym == Any_isInstanceOf) ||
      (sym == Object_isInstanceOf) ||
      (sym == Object_==) ||
      (sym == Object_!=) ||
      (sym == Object_##)
    )

    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol

      def mayNeedProtectedAccessor(sel: Select, args: List[Tree], goToSuper: Boolean) =
        if (needsProtectedAccessor(sym, tree.pos)) {
          if (settings.debug.value)
            log("Adding protected accessor for " + tree)

          transform(makeAccessor(sel, args))
        }
        else if (goToSuper) super.transform(tree)
        else tree

      try tree match {
        case ClassDef(_, _, _, _) =>
          checkCompanionNameClashes(sym)
          val decls = sym.info.decls
          for (s <- decls.toList) {
            if (s.privateWithin.isClass && !s.privateWithin.isModuleClass &&
                !s.hasFlag(EXPANDEDNAME) && !s.isConstructor) {
              decls.unlink(s)
              s.expandName(s.privateWithin)
              decls.enter(s)
            }
          }
          if (settings.verbose.value && onlyPresentation && !sym.isAnonymousClass) {
            println("========== scaladoc of "+sym+" =============================")
            println(toJavaDoc(expandedDocComment(sym)))
            for (member <- sym.info.members) {
              println(member+":"+sym.thisType.memberInfo(member)+"\n"+
                      toJavaDoc(expandedDocComment(member, sym)))
              for ((useCase, comment, pos) <- useCases(member, sym)) {
                println("usecase "+useCase+":"+useCase.info)
                println(toJavaDoc(comment))
              }
            }
          }
          super.transform(tree)
        case ModuleDef(_, _, _) =>
          checkCompanionNameClashes(sym)
          super.transform(tree)
        case Template(parents, self, body) =>
          val ownAccDefs = new ListBuffer[Tree];
          accDefs = (currentOwner, ownAccDefs) :: accDefs;

          // ugly hack... normally, the following line should not be
          // necessary, the 'super' method taking care of that. but because
          // that one is iterating through parents (and we dont want that here)
          // we need to inline it.
          curTree = tree
          val body1 = atOwner(currentOwner) { transformTrees(body) }
          accDefs = accDefs.tail;
          treeCopy.Template(tree, parents, self, ownAccDefs.toList ::: body1);

        case TypeApply(sel @ Select(This(_), name), args) =>
          mayNeedProtectedAccessor(sel, args, false)

        case sel @ Select(qual @ This(_), name) =>
           if ((sym hasFlag PARAMACCESSOR)
               && (sym.alias != NoSymbol)) {
            val result = localTyper.typed {
                Select(
                  Super(qual.symbol, nme.EMPTY.toTypeName/*qual.symbol.info.parents.head.symbol.name*/) setPos qual.pos,
                  sym.alias) setPos tree.pos
            }
            if (settings.debug.value)
              Console.println("alias replacement: " + tree + " ==> " + result);//debug
            localTyper.typed(gen.maybeMkAsInstanceOf(transformSuperSelect(result), sym.tpe, sym.alias.tpe, true))
          }
          else mayNeedProtectedAccessor(sel, List(EmptyTree), false)

        case Select(sup @ Super(_, mix), name) =>
          if (sym.isValue && !sym.isMethod || sym.hasFlag(ACCESSOR)) {
            unit.error(tree.pos, "super may be not be used on "+
                       (if (sym.hasFlag(ACCESSOR)) sym.accessed else sym))
          }
          else if (isDisallowed(sym)) {
            unit.error(tree.pos, "super not allowed here: use this." + name.decode + " instead")
          }
          transformSuperSelect(tree)

        case TypeApply(sel @ Select(qual, name), args) =>
          mayNeedProtectedAccessor(sel, args, true)

        case sel @ Select(qual, name) =>
          mayNeedProtectedAccessor(sel, List(EmptyTree), true)

        case Assign(lhs @ Select(qual, name), rhs) =>
          if (lhs.symbol.isVariable &&
              lhs.symbol.hasFlag(JAVA) &&
              needsProtectedAccessor(lhs.symbol, tree.pos)) {
            if (settings.debug.value) log("Adding protected setter for " + tree)
            val setter = makeSetter(lhs);
            if (settings.debug.value)
              log("Replaced " + tree + " with " + setter);
            transform(localTyper.typed(Apply(setter, List(qual, rhs))))
          } else
            super.transform(tree)

        case Apply(fn, args) =>
          assert(fn.tpe != null, tree)
          treeCopy.Apply(tree, transform(fn), transformArgs(args, fn.tpe.params))
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

    override def atOwner[A](owner: Symbol)(trans: => A): A = {
      if (owner.isClass) validCurrentOwner = true
      super.atOwner(owner)(trans)
    }

    private def withInvalidOwner[A](trans: => A): A = {
      val prevValidCurrentOwner = validCurrentOwner
      validCurrentOwner = false
      val result = trans
      validCurrentOwner = prevValidCurrentOwner
      result
    }

    /** Add a protected accessor, if needed, and return a tree that calls
     *  the accessor and returns the the same member. The result is already
     *  typed.
     */
    private def makeAccessor(tree: Select, targs: List[Tree]): Tree = {
      val Select(qual, name) = tree
      val sym = tree.symbol
      val clazz = hostForAccessorOf(sym, currentOwner.enclClass)

      /** Return a list of list of types of all value parameter sections. */
      def allParamTypes(tpe: Type): List[List[Type]] = tpe match {
        case PolyType(_, restpe) => allParamTypes(restpe)
        case MethodType(params, res) => params.map(_.tpe) :: allParamTypes(res)
        case _ => Nil
      }


      assert(clazz != NoSymbol, sym)
      if (settings.debug.value)  log("Decided for host class: " + clazz)

      val accName = nme.protName(sym.originalName)
      val hasArgs = sym.tpe.paramTypes != Nil
      val memberType = refchecks.toScalaRepeatedParam(sym.tpe) // fix for #2413

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
      if (settings.debug.value) log("accType: " + accType)

      var protAcc = clazz.info.decl(accName).suchThat(s => s == NoSymbol || s.tpe =:= accType(s))
      if (protAcc == NoSymbol) {
        protAcc = clazz.newMethod(tree.pos, nme.protName(sym.originalName))
        protAcc.setInfo(accType(protAcc))
        clazz.info.decls.enter(protAcc);
        val code = DefDef(protAcc, {
          val obj = protAcc.paramss.head.head // receiver
          protAcc.paramss.tail.zip(allParamTypes(sym.tpe)).foldLeft(Select(Ident(obj), sym): Tree) (
              (fun, pvparams) => {
                Apply(fun, (pvparams._1, pvparams._2).zipped map (makeArg(_, obj, _)))
              })
        })

        if (settings.debug.value)
          log(code)
        accDefBuf(clazz) += typers(clazz).typed(code)
      }
      var res: Tree = atPos(tree.pos) {
        if (targs.head == EmptyTree)
          Apply(Select(This(clazz), protAcc), List(qual))
        else
          Apply(TypeApply(Select(This(clazz), protAcc), targs), List(qual))
      }
      if (settings.debug.value)
        log("Replaced " + tree + " with " + res)
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
    private def makeArg(v: Symbol, obj: Symbol, expectedTpe: Type): Tree = {
      var res: Tree = Ident(v)
      val sym = obj.tpe.typeSymbol
      var ownerClass: Symbol = NoSymbol

      val isDependentType = expectedTpe match {
        case TypeRef(path, _, _) =>
          ownerClass = thisTypeOfPath(path)
          if (sym.isSubClass(ownerClass)) true else false
        case _ => false
      }
      if (definitions.isRepeatedParamType(v.info)) {
        res = gen.wildcardStar(res)
        log("adapted to wildcard star: " + res)
      }
      if (isDependentType) {
        val preciseTpe = expectedTpe.asSeenFrom(singleType(NoPrefix, obj), ownerClass) //typeRef(singleType(NoPrefix, obj), v.tpe.symbol, List())
        TypeApply(Select(res, Any_asInstanceOf),
                  List(TypeTree(preciseTpe)))
      } else res
    }

    /** For a path-dependent type, return the this type. */
    private def thisTypeOfPath(path: Type): Symbol = path match {
      case ThisType(outerSym)  => outerSym
      case SingleType(rest, _) => thisTypeOfPath(rest)
      case _ => NoSymbol
    }

    /** Add an accessor for field, if needed, and return a selection tree for it .
     *  The result is not typed.
     */
    private def makeSetter(tree: Select): Tree = {
      val field = tree.symbol
      val clazz = hostForAccessorOf(field, currentOwner.enclClass)
      assert(clazz != NoSymbol, field)
      if (settings.debug.value)
        log("Decided for host class: " + clazz)
      val accName = nme.protSetterName(field.originalName)
      var protAcc = clazz.info.decl(accName)
      if (protAcc == NoSymbol) {
        protAcc = clazz.newMethod(field.pos, nme.protSetterName(field.originalName))
        protAcc.setInfo(MethodType(protAcc.newSyntheticValueParams(List(clazz.typeOfThis, field.tpe)),
                                   UnitClass.tpe))
        clazz.info.decls.enter(protAcc)
        val code = DefDef(protAcc, {
          val obj :: value :: Nil = protAcc.paramss.head;
          atPos(tree.pos) {
            Assign(
              Select(Ident(obj), field.name),
              Ident(value))
          }
        })
        if (settings.debug.value)
          log(code);
        accDefBuf(clazz) += typers(clazz).typed(code)
      }
      var res: Tree = atPos(tree.pos) { Select(This(clazz), protAcc) }
      res
    }

    /** Does `sym' need an accessor when accessed from `currentOwner'?
     *  A special case arises for classes with explicit self-types. If the
     *  self type is a Java class, and a protected accessor is needed, we issue
     *  an error. If the self type is a Scala class, we don't add an accessor.
     *  An accessor is not needed if the access boundary is larger than the
     *  enclosing package, since that translates to 'public' on the host system.
     *  (as Java has no real package nesting).
     *
     * If the access happens inside a 'trait', access is more problematic since
     * the implementation code is moved to an '$class' class which does not
     * inherit anything. Since we can't (yet) add accessors for 'required'
     * classes, this has to be signaled as error.
     */
    private def needsProtectedAccessor(sym: Symbol, pos: Position): Boolean = {
      def errorRestriction(msg: String) {
        unit.error(pos, "Implementation restriction: " + msg)
      }

      def accessibleThroughSubclassing: Boolean =
        (validCurrentOwner
            && currentOwner.enclClass.thisSym.isSubClass(sym.owner)
            && !currentOwner.enclClass.isTrait)

      val res = /* settings.debug.value && */
      ((sym hasFlag PROTECTED)
       && sym.hasFlag(JAVA)
       && !sym.owner.isPackageClass
       && !accessibleThroughSubclassing
       && (sym.owner.enclosingPackageClass != currentOwner.enclosingPackageClass)
       && (sym.owner.enclosingPackageClass == sym.accessBoundary(sym.owner).enclosingPackageClass))

      if (res) {
        val host = hostForAccessorOf(sym, currentOwner.enclClass)
        // bug #1393 - as things stand now the "host" could be a package.
        if (host.isPackageClass) false
        else if (host.thisSym != host) {
          if (host.thisSym.tpe.typeSymbol.hasFlag(JAVA))
            errorRestriction("%s accesses protected %s from self type %s.".format(currentOwner.enclClass, sym, host.thisSym.tpe))
          false
        } else if (host.isTrait && sym.hasFlag(JAVA)) {
            errorRestriction(("%s accesses protected %s inside a concrete trait method. " +
                    "Add an accessor in a class extending %s to work around this bug.").format(currentOwner.enclClass, sym, sym.enclClass))
            false
        } else res
      } else res
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
        assert(referencingClass.isClass)
        referencingClass
      } else if(referencingClass.owner.enclClass != NoSymbol)
        hostForAccessorOf(sym, referencingClass.owner.enclClass)
      else referencingClass
    }

    /** Is 'tpe' the type of a member of an enclosing class? */
    private def isThisType(tpe: Type): Boolean = tpe match {
      case ThisType(sym) => (sym.isClass && !sym.isPackageClass)
      case TypeRef(pref, _, _) => isThisType(pref)
      case SingleType(pref, _) => isThisType(pref)
      case RefinedType(parents, defs) =>
        parents.exists(isThisType(_))
      case AnnotatedType(_, tp, _) =>
        isThisType(tp)
      case _ => false
    }
  }
}
