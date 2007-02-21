/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

import scala.collection.mutable.ListBuffer
import nsc.symtab.Flags._

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
  import posAssigner.atPos
  import typer.typed

  /** the following two members override abstract members in Transform */
  val phaseName: String = "superaccessors"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new SuperAccTransformer(unit)

  class SuperAccTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    private var validCurrentOwner = true
    private var accDefs: List[(Symbol, ListBuffer[Tree])] = List()

    private def accDefBuf(clazz: Symbol) =
      accDefs.dropWhile(._1.!=(clazz)).head._2

    private def transformArgs(args: List[Tree], formals: List[Type]) = {
      if (!formals.isEmpty && formals.last.symbol == definitions.ByNameParamClass)
        ((args take (formals.length - 1) map transform) :::
         withInvalidOwner { args drop (formals.length - 1) map transform })
      else
        args map transform
    }

    override def transform(tree: Tree): Tree = tree match {
      case ClassDef(_, _, _, _, _) =>
        val decls = tree.symbol.info.decls
        for (val sym <- decls.toList) {
          if (sym.privateWithin.isClass && !sym.privateWithin.isModuleClass &&
              !sym.hasFlag(EXPANDEDNAME)) {
            decls.unlink(sym)
            sym.expandName(sym.privateWithin)
            decls.enter(sym)
          }
        }
        super.transform(tree)
      case Template(parents, body) =>
	val ownAccDefs = new ListBuffer[Tree];
	accDefs = (currentOwner, ownAccDefs) :: accDefs;

        // ugly hack... normally, the following line should not be
        // necessary, the 'super' method taking care of that. but because
        // that one is iterating through parents (and we dont want that here)
        // we need to inline it.
        curTree = tree
        val body1 = atOwner(currentOwner) { transformTrees(body) }
	accDefs = accDefs.tail;
	copy.Template(tree, parents, ownAccDefs.toList ::: body1);

      case Select(qual @ This(_), name) =>
        val sym = tree.symbol
         if ((sym hasFlag PARAMACCESSOR) && (sym.alias != NoSymbol)) {
          val result = typed {
            Select(
              Super(qual.symbol, nme.EMPTY.toTypeName/*qual.symbol.info.parents.head.symbol.name*/) setPos qual.pos,
              sym.alias) setPos tree.pos
          }
          if (settings.debug.value)
            Console.println("alias replacement: " + tree + " ==> " + result);//debug
          transform(result)
        } else {
          if (needsProtectedAccessor(sym, tree.pos)) {
            if (settings.debug.value) log("Adding protected accessor for " + tree);
            transform(makeAccessor(tree.asInstanceOf[Select]))
          } else
            tree
        }
      case Select(sup @ Super(_, mix), name) =>
        val sym = tree.symbol
        val clazz = sup.symbol
        if (sym hasFlag DEFERRED) {
          val member = sym.overridingSymbol(clazz);
          if (mix != nme.EMPTY.toTypeName || member == NoSymbol ||
              !((member hasFlag ABSOVERRIDE) && member.isIncompleteIn(clazz)))
            unit.error(tree.pos, ""+sym+sym.locationString+" is accessed from super. It may not be abstract "+
                                 "unless it is overridden by a member declared `abstract' and `override'");
        }
        if (tree.isTerm && mix == nme.EMPTY.toTypeName &&
            (clazz.isTrait || clazz != currentOwner.enclClass || !validCurrentOwner)) {
          val supername = nme.superName(sym.name)
          var superAcc = clazz.info.decl(supername).suchThat(.alias.==(sym))
          if (superAcc == NoSymbol) {
            if (settings.debug.value) log("add super acc " + sym + sym.locationString + " to `" + clazz);//debug
            superAcc =
              clazz.newMethod(tree.pos, supername)
                .setFlag(SUPERACCESSOR | PRIVATE)
                .setInfo(clazz.thisType.memberType(sym))
                .setAlias(sym)
            clazz.info.decls enter superAcc;
            accDefBuf(clazz) += typed(DefDef(superAcc, vparamss => EmptyTree))
          }
          atPos(sup.pos) {
            Select(gen.mkAttributedThis(clazz), superAcc) setType tree.tpe;
          }
        } else tree

      case Select(qual, name) =>
        val sym = tree.symbol
        if (needsProtectedAccessor(sym, tree.pos)) {
          if (settings.debug.value) log("Adding protected accessor for tree: " + tree);
          transform(makeAccessor(tree.asInstanceOf[Select]))
        } else
          super.transform(tree)

      case Assign(lhs @ Select(qual, name), rhs) =>
        if (lhs.symbol.isVariable &&
            lhs.symbol.hasFlag(JAVA) &&
            needsProtectedAccessor(lhs.symbol, tree.pos)) {
          if (settings.debug.value) log("Adding protected setter for " + tree)
          val setter = makeSetter(lhs);
          if (settings.debug.value)
            log("Replaced " + tree + " with " + setter);
          transform(typed(Apply(setter, List(qual, rhs))))
        } else
          super.transform(tree)

      case Apply(fn, args) =>
        copy.Apply(tree, transform(fn), transformArgs(args, fn.tpe.paramTypes))
      case Function(vparams, body) =>
        withInvalidOwner {
          copy.Function(tree, vparams, transform(body))
        }
      case _ =>
        super.transform(tree)
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
    private def makeAccessor(tree: Select): Tree = {
      val Select(qual, name) = tree
      val sym = tree.symbol
      val clazz = hostForAccessorOf(sym, currentOwner.enclClass)

      /** Return a list of list of types of all value parameter sections. */
      def allParamTypes(tpe: Type): List[List[Type]] = tpe match {
        case MethodType(pts, res) => pts :: allParamTypes(res)
        case _ => Nil
      }

      assert(clazz != NoSymbol, sym)
      if (settings.debug.value)
        log("Decided for host class: " + clazz)
      val accName = nme.protName(sym.originalName)
      var protAcc = clazz.info.decl(accName)
      val hasArgs = sym.tpe.paramTypes != Nil
      if (protAcc == NoSymbol) {
        val argTypes = tree.tpe // transform(sym.tpe)
        // if the result type depends on the this type of an enclosing class, the accessor
        // has to take an object of exactly this type, otherwise it's more general
	val objType = if (isThisType(argTypes.finalResultType)) clazz.thisType else clazz.typeOfThis

        protAcc = clazz.newMethod(tree.pos, nme.protName(sym.originalName))
                           .setInfo(MethodType(List(objType),argTypes))
        clazz.info.decls.enter(protAcc);
        val code = DefDef(protAcc, vparamss => {
          val obj = vparamss.head.head
          vparamss.tail.zip(allParamTypes(sym.tpe)).foldLeft(Select(Ident(obj), sym): Tree) (
              (fun, pvparams) => {
                Apply(fun, (List.map2(pvparams._1, pvparams._2) { (v, origTpe) => makeArg(v, obj, origTpe) } ))
              })
        })

        if (settings.debug.value)
          log(code)
        accDefBuf(clazz) += typers(clazz).typed(code)
      }
      var res: Tree = atPos(tree.pos) { Apply(Select(This(clazz), protAcc), List(qual)) }
      if (settings.debug.value)
        log("Replaced " + tree + " with " + res)
      if (hasArgs) typer.typedOperator(res) else typer.typed(res)
    }

    /** Adapt the given argument in call to protected member.
     *  Adaptation means adding a cast to a path-dependent type, for instance
     *
     *  def prot$m(obj: Outer)(x: Inner) = obj.m(x.asInstanceOf[obj.Inner]).
     *
     *  such a cast might be necessary when m expects an Outer.this.Inner (the
     *  outer of 'obj' and 'x' have to be the same). This restriction can't be
     *  expressed in the type system (but is implicit when defining method m).
     */
    private def makeArg(v: Symbol, obj: Symbol, expectedTpe: Type): Tree = {
      val res = Ident(v)
      val sym = obj.tpe.symbol
      var ownerClass: Symbol = NoSymbol

      val isDependentType = expectedTpe match {
        case TypeRef(path, _, _) =>
          ownerClass = thisTypeOfPath(path)
          if (sym.isSubClass(ownerClass)) true else false
        case _ => false
      }
      if (isDependentType) {
        val preciseTpe = expectedTpe.asSeenFrom(singleType(NoPrefix, obj), ownerClass) //typeRef(singleType(NoPrefix, obj), v.tpe.symbol, List())
        TypeApply(Select(res, definitions.Any_asInstanceOf),
                  List(TypeTree(preciseTpe)))
      }
      else
        res
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
                           .setInfo(MethodType(List(clazz.typeOfThis, field.tpe), definitions.UnitClass.tpe));
        clazz.info.decls.enter(protAcc)
        val code = DefDef(protAcc, vparamss => {
          val obj :: value :: Nil = vparamss.head;
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
     *
     * If the access happens inside a 'trait', access is more problematic since
     * the implementation code is moved to an '$class' class which does not
     * inherit anything. Since we can't (yet) add accessors for 'required'
     * classes, this has to be signaled as error.
     */
    private def needsProtectedAccessor(sym: Symbol, pos: PositionType): Boolean = {
      val res = /* settings.debug.value && */
      ((sym hasFlag PROTECTED)
       && (!validCurrentOwner || !(currentOwner.enclClass.thisSym isSubClass sym.owner))
       && (enclPackage(sym.owner) != enclPackage(currentOwner)))

      if (res) {
        val host = hostForAccessorOf(sym, currentOwner.enclClass)
        if (host.thisSym != host) {
          if (host.thisSym.tpe.symbol.hasFlag(JAVA) || currentOwner.enclClass.isTrait)
            unit.error(pos, "Implementation restriction: " + currentOwner.enclClass + " accesses protected "
                            + sym + " from 'required' " + host.thisSym.tpe)
          false
        } else res
      } else res
    }

    /** Return the enclosing package of the given symbol. */
    private def enclPackage(sym: Symbol): Symbol =
      if ((sym == NoSymbol) || sym.isPackageClass) sym else enclPackage(sym.owner)

    /** Return the innermost enclosing class C of referencingClass for which either
     *  of the following holds:
     *     - C is a subclass of sym.owner or
     *     - C is declared in the same package as sym's owner
     */
    private def hostForAccessorOf(sym: Symbol, referencingClass: Symbol): Symbol = {
      if (referencingClass.isSubClass(sym.owner.enclClass)
          || referencingClass.thisSym.isSubClass(sym.owner.enclClass)
          || enclPackage(referencingClass) == enclPackage(sym.owner)) {
        assert(referencingClass.isClass)
        referencingClass
      } else
        hostForAccessorOf(sym, referencingClass.owner.enclClass)
    }

    /** Is 'tpe' the type of a member of an enclosing class? */
    private def isThisType(tpe: Type): Boolean = tpe match {
      case ThisType(sym) => (sym.isClass && !sym.isPackageClass)
      case TypeRef(pref, _, _) => isThisType(pref)
      case SingleType(pref, _) => isThisType(pref)
      case _ => false
    }
  }
}
