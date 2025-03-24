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

package scala
package tools.nsc
package transform

import symtab._
import Flags.{CASE => _, _}
import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.Reporting.WarningCategory

/** This class ...
 *
 *  @author  Martin Odersky
 */
abstract class ExplicitOuter extends InfoTransform
      with TypingTransformers
      with ast.TreeDSL
{
  import global._
  import definitions._
  import CODE._

  /** The following flags may be set by this phase: */
  override def phaseNewFlags: Long = notPROTECTED

  /** the name of the phase: */
  val phaseName: String = "explicitouter"

  /** This class does not change linearization */
  override def changesBaseClasses = false

  protected def newTransformer(unit: CompilationUnit): AstTransformer =
    new ExplicitOuterTransformer(unit)

  /** Is given clazz an inner class? */
  private def isInner(clazz: Symbol) =
    !clazz.isPackageClass && !clazz.outerClass.isStaticOwner

  private def haveSameOuter(parent: Type, clazz: Symbol) = {
    val owner = clazz.owner
    val parentSym = parent.typeSymbol

    parentSym.isClass && owner.isClass &&
      (owner isSubClass parentSym.owner) &&
      owner.thisType =:= parent.prefix
  }

  /** Does given clazz define an outer field? */
  def hasOuterField(clazz: Symbol) = {
    val parent = clazz.info.firstParent

    // space optimization: inherit the $outer pointer from the parent class if
    // we know that it will point to the correct instance.
    def canReuseParentOuterField = !parent.typeSymbol.isJavaDefined && haveSameOuter(parent, clazz)

    isInner(clazz) && !clazz.isTrait && !canReuseParentOuterField
  }

  private def outerField(clazz: Symbol): Symbol = {
    val result = clazz.info.member(nme.OUTER_LOCAL)
    assert(result != NoSymbol, "no outer field in "+clazz+" at "+phase)

    result
  }

  class RemoveBindingsTransformer(toRemove: Set[Symbol]) extends AstTransformer {
    override def transform(tree: Tree) = tree match {
      case Bind(_, body) if toRemove(tree.symbol) => super.transform(body)
      case _                                      => super.transform(tree)
    }
  }

  def outerAccessor(clazz: Symbol): Symbol = {
    val firstTry = clazz.info.decl(nme.expandedName(nme.OUTER, clazz))
    if (firstTry != NoSymbol && firstTry.outerSource == clazz) firstTry
    else findOrElse(clazz.info.decls)(_.outerSource == clazz)(NoSymbol)
  }
  def newOuterAccessor(clazz: Symbol) = {
    val accFlags = SYNTHETIC | ARTIFACT | STABLE | ( if (clazz.isTrait) DEFERRED else 0 )
    val sym      = clazz.newMethod(nme.OUTER, clazz.pos, accFlags)
    val restpe   = if (clazz.isTrait) clazz.outerClass.tpe_* else clazz.outerClass.thisType

    sym expandName clazz
    sym.referenced = clazz
    sym setInfo MethodType(Nil, restpe)
  }
  def newOuterField(clazz: Symbol) = {
    val accFlags = SYNTHETIC | ARTIFACT | PARAMACCESSOR | ( if (clazz.isEffectivelyFinal) PrivateLocal else PROTECTED )
    val sym      = clazz.newValue(nme.OUTER_LOCAL, clazz.pos, accFlags)

    sym setInfo clazz.outerClass.thisType
  }

  /**
   * Will the outer accessor of the `clazz` subsume the outer accessor of
   * `mixin`?
   *
   * This arises when an inner object mixes in its companion trait.
   *
   * {{{
   *   class C {
   *     trait T { C.this }            // C\$T\$\$\$outer\$ : C
   *     object T extends T { C.this } // C\$T\$\$\$outer\$ : C.this.type
   *   }
   * }}}
   *
   * See scala/bug#7242.
   }}
   */
  private def skipMixinOuterAccessor(clazz: Symbol, mixin: Symbol) = {
    // Reliant on the current scheme for name expansion, the expanded name
    // of the outer accessors in a trait and its companion object are the same.
    // If the assumption is one day falsified, run/t7424.scala will let us know.
    clazz.fullName == mixin.fullName
  }

  /** <p>
   *    The type transformation method:
   *  </p>
   *  <ol>
   *    <li>
   *      Add an outer parameter to the formal parameters of a constructor
   *      in an inner non-trait class;
   *    </li>
   *    <li>
   *      Add a protected \$outer field to an inner class which is
   *      not a trait.
   *    </li>
   *    <li>
   *      <p>
   *        Add an outer accessor \$outer\$\$C to every inner class
   *        with fully qualified name C that is not an interface.
   *        The outer accessor is abstract for traits, concrete for other
   *        classes.
   *      </p>
   *      <p>
   *        3a. Also add overriding accessor defs to every class that inherits
   *        mixin classes with outer accessor defs (unless the superclass
   *        already inherits the same mixin).
   *      </p>
   *    </li>
   *    <li>
   *      Make all super accessors and modules in traits non-private, mangling
   *      their names.
   *    </li>
   *    <li>
   *      Remove protected flag from all members of traits.
   *    </li>
   *  </ol>
   *  Note: this transformInfo need not be reflected as the JVM reflection already
   *  elides outer pointers.
   */
  def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case MethodType(params, resTp) =>
      val resTpTransformed = transformInfo(sym, resTp)

      val paramsWithOuter =
        if (sym.isClassConstructor && isInner(sym.owner)) // 1
          sym.newValueParameter(nme.OUTER_ARG, sym.pos, ARTIFACT).setInfo(sym.owner.outerClass.thisType) :: params
        else params

      if ((resTpTransformed ne resTp) || (paramsWithOuter ne params)) MethodType(paramsWithOuter, resTpTransformed)
      else tp

    case ClassInfoType(parents, decls, clazz) if !clazz.isJava =>
      var decls1 = decls
      if (isInner(clazz) && !clazz.isInterface) {
        decls1 = decls.cloneScope
        decls1 enter newOuterAccessor(clazz) // 3
        if (hasOuterField(clazz)) //2
          decls1 enter newOuterField(clazz)
      }
      if (!clazz.isTrait && !parents.isEmpty) {
        for (mc <- clazz.mixinClasses) {
          val mixinOuterAcc: Symbol = exitingExplicitOuter(outerAccessor(mc))
          if (mixinOuterAcc != NoSymbol) {
            if (skipMixinOuterAccessor(clazz, mc))
              debuglog(s"Reusing outer accessor symbol of $clazz for the mixin outer accessor of $mc")
            else {
              if (decls1 eq decls) decls1 = decls.cloneScope
              val newAcc = mixinOuterAcc.cloneSymbol(clazz, mixinOuterAcc.flags & ~DEFERRED).setPos(clazz.pos)
              newAcc setInfo (clazz.thisType memberType mixinOuterAcc)
              decls1 enter newAcc
            }
          }
        }
      }
      if (decls1 eq decls) tp else ClassInfoType(parents, decls1, clazz)
    case PolyType(tparams, restp) =>
      val restp1 = transformInfo(sym, restp)
      if (restp eq restp1) tp else PolyType(tparams, restp1)

    case _ =>
      tp
  }

  /** A base class for transformers that maintain outerParam
   *  values for outer parameters of constructors.
   *  The class provides methods for referencing via outer.
   */
  abstract class OuterPathTransformer(initLocalTyper: analyzer.Typer) extends TypingTransformer(initLocalTyper) {
    def this(unit: CompilationUnit) = this(newRootLocalTyper(unit))
    /** The directly enclosing outer parameter, if we are in a constructor */
    protected var outerParam: Symbol = NoSymbol

    /** The first outer selection from currently transformed tree.
     *  The result is typed but not positioned.
     *
     * Will return `EmptyTree` if there is no outer accessor because of a premature self reference.
     */
    protected def outerValue: Tree = outerParam match {
      case NoSymbol   => outerSelect(gen.mkAttributedThis(currentClass))
      case outerParam => gen.mkAttributedIdent(outerParam)
    }

    /** Select and apply outer accessor from 'base'
     *  The result is typed but not positioned.
     *  If the outer access is from current class and current class is final
     *  take outer field instead of accessor
     *
     *  Will return `EmptyTree` if there is no outer accessor because of a premature self reference.
     */
    private def outerSelect(base: Tree): Tree = {
      val baseSym = base.tpe.typeSymbol
      val outerAcc = outerAccessor(baseSym)
      if (outerAcc == NoSymbol) {
        if (baseSym.ownersIterator.exists(isUnderConstruction)) {
          // e.g neg/t6666.scala
          // The caller will report the error with more information.
          EmptyTree
        } else {
          globalError(currentOwner.pos, s"Internal error: unable to find the outer accessor symbol of $baseSym")
          EmptyTree
        }
      } else {
        val currentClass = this.currentClass //todo: !!! if this line is removed, we get a build failure that protected$currentClass need an override modifier
        // outerFld is the $outer field of the current class, if the reference can
        // use it (i.e. reference is allowed to be of the form this.$outer),
        // otherwise it is NoSymbol
        val outerFld =
          if (outerAcc.owner == currentClass &&
            !outerAcc.owner.isTrait &&
            base.tpe =:= currentClass.thisType &&
            outerAcc.owner.isEffectivelyFinal)
            outerField(currentClass) suchThat (_.owner == currentClass)
          else
            NoSymbol
        val path =
          if (outerFld != NoSymbol) Select(base, outerFld)
          else Apply(Select(base, outerAcc), Nil)

        localTyper typed path
      }
    }

    /** The path
     *  <blockquote><pre>`base`.\$outer\$\$C1 ... .\$outer\$\$Cn</pre></blockquote>
     *  which refers to the outer instance of class to of
     *  value base. The result is typed but not positioned.
     */
    @tailrec
    protected final def outerPath(base: Tree, from: Symbol, to: Symbol): Tree = {
      //Console.println("outerPath from "+from+" to "+to+" at "+base+":"+base.tpe)
      if (from == to) base
      else {
        val outerSel = outerSelect(base)
        if (outerSel.isEmpty) EmptyTree
        else outerPath(outerSel, from.outerClass, to)
      }
    }


    /** The stack of class symbols in which a call to this() or to the super
      * constructor, or early definition is active
      */
    protected def isUnderConstruction(clazz: Symbol) = selfOrSuperCalls contains clazz
    protected val selfOrSuperCalls = collection.mutable.Stack[Symbol]()

    override def transform(tree: Tree): Tree = {
      def sym = tree.symbol
      val savedOuterParam = outerParam
      try {
        tree match {
          case Template(_, _, _) =>
            outerParam = NoSymbol
          case DefDef(_, _, _, (param :: _) :: _, _, _) if sym.isClassConstructor && isInner(sym.owner) =>
            outerParam = param.symbol
            assert(outerParam.name startsWith nme.OUTER, outerParam.name)
          case _ =>
        }
        if ((treeInfo isSelfOrSuperConstrCall tree) || (treeInfo isEarlyDef tree)) {
          selfOrSuperCalls push currentOwner.owner
          val transformed = super.transform(tree)
          selfOrSuperCalls.pop()
          transformed
        } else
          super.transform(tree)
      }
      finally outerParam = savedOuterParam
    }
  }

  /** The phase performs the following transformations (more or less...):
    *
    * (1) An class which is not an interface and is not static gets an outer accessor (@see outerDefs).
    * (1a) A class which is not a trait gets an outer field.
    *
    * (4) A constructor of a non-trait inner class gets an outer parameter.
    *
    * (5) A reference C.this where C refers to an outer class is replaced by a selection
    *     `this.\$outer\$\$C1 ... .\$outer\$\$Cn` (@see outerPath)
    *
    * (7) A call to a constructor Q.(args) or Q.\$init\$(args) where Q != this and
    *     the constructor belongs to a non-static class is augmented by an outer argument.
    *     E.g. Q.(OUTER, args) where OUTER
    *     is the qualifier corresponding to the singleton type Q.
    *
    * (8) A call to a constructor this.(args) in a
    *     secondary constructor is augmented to this.(OUTER, args)
    *     where OUTER is the last parameter of the secondary constructor.
    *
    * (9) Remove private modifier from class members M that are accessed from an inner class.
    *
    * (10) Remove protected modifier from class members M that are accessed
    *      without a super qualifier accessed from an inner class or trait.
    *
    * (11) Remove private and protected modifiers from type symbols
    *
    * Note: The whole transform is run in phase explicitOuter.next.
    *
    * TODO: Make this doc reflect what's actually going on.
    *       Some of the deviations are motivated by separate compilation
    *       (name mangling based on usage is inherently unstable).
    *       Now that traits are compiled 1:1 to interfaces, they can have private members,
    *       so there's also less need to make trait members non-private
    *       (they still may need to be implemented in subclasses, though we could make those protected...).
    */
  class ExplicitOuterTransformer(unit: CompilationUnit) extends OuterPathTransformer(unit) {
    transformer =>

    /** The definition tree of the outer accessor of current class
     */
    def outerFieldDef: Tree = ValDef(outerField(currentClass))

    /** The definition tree of the outer accessor of current class
     */
    def outerAccessorDef: Tree = localTyper typed {
      val acc = outerAccessor(currentClass)
      val rhs = if (acc.isDeferred) EmptyTree else Select(This(currentClass), outerField(currentClass))
      DefDef(acc, rhs)
    }

    /** The definition tree of the outer accessor for class mixinClass.
     *
     *  @param mixinClass The mixin class which defines the abstract outer
     *                    accessor which is implemented by the generated one.
     *  @note Pre-condition: `mixinClass` is an inner class
     */
    def mixinOuterAccessorDef(mixinClass: Symbol): Tree = {
      val outerAcc    = outerAccessor(mixinClass) overridingSymbol currentClass
      def mixinPrefix = (currentClass.thisType baseType mixinClass).prefix
      assert(outerAcc != NoSymbol, "No outer accessor for inner mixin " + mixinClass + " in " + currentClass)
      assert(outerAcc.alternatives.size == 1, s"Multiple outer accessors match inner mixin $mixinClass in $currentClass : ${outerAcc.alternatives.map(_.defString)}")
      // I added the mixinPrefix.typeArgs.nonEmpty condition to address the
      // crash in scala/bug#4970.  I feel quite sure this can be improved.
      val path = (
        if (mixinClass.owner.isTerm) gen.mkAttributedThis(mixinClass.owner.enclClass)
        else if (mixinPrefix.typeArgs.nonEmpty) gen.mkAttributedThis(mixinPrefix.typeSymbol)
        else gen.mkAttributedQualifier(mixinPrefix)
      )
      // Need to cast for nested outer refs in presence of self-types. See ticket #3274.
      localTyper typed DefDef(outerAcc, gen.mkCast(transformer.transform(path), outerAcc.info.resultType))
    }

    /** The main transformation method */
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      if (sym != null && sym.isType) { // (9)
        if (sym.isPrivate) sym setFlag notPRIVATE
        if (sym.isProtected && !sym.isJavaDefined) sym setFlag notPROTECTED
      }
      tree match {
        case Template(_, _, _) =>
          val newDefs = new ListBuffer[Tree]
          atOwner(tree, currentOwner) {
            if (!currentClass.isInterface) {
              if (isInner(currentClass)) {
                if (hasOuterField(currentClass))
                  newDefs += outerFieldDef // (1a)
                newDefs += outerAccessorDef // (1)
              }
              if (!currentClass.isTrait)
                for (mc <- currentClass.mixinClasses)
                  if (outerAccessor(mc) != NoSymbol && !skipMixinOuterAccessor(currentClass, mc))
                    newDefs += mixinOuterAccessorDef(mc)
            }
          }
          super.transform(
            deriveTemplate(tree)(decls =>
              if (newDefs.isEmpty) decls
              else decls ::: newDefs.toList
            )
          )
        case DefDef(_, _, _, vparamss, _, rhs) =>
          if (sym.isClassConstructor) {
            val clazz = sym.owner
            val vparamss1 =
              if (isInner(clazz)) { // (4)
                if (isUnderConstruction(clazz.outerClass)) {
                  reporter.error(tree.pos, s"Implementation restriction: ${clazz.fullLocationString} requires premature access to ${clazz.outerClass}.")
                }
                val outerParam =
                  sym.newValueParameter(nme.OUTER, sym.pos, ARTIFACT) setInfo clazz.outerClass.thisType
                ((ValDef(outerParam) setType NoType) :: vparamss.head) :: vparamss.tail
              } else vparamss
            super.transform(copyDefDef(tree)(vparamss = vparamss1))
          } else
            super.transform(tree)

        case This(qual) =>
          if (sym == currentClass || sym.hasModuleFlag && sym.isStatic) tree
          else atPos(tree.pos)(outerPath(outerValue, currentClass.outerClass, sym)) // (5)

        case Select(qual, name) =>
          // make not private symbol accessed from inner classes, as well as
          // symbols accessed from @inline methods
          //
          // See scala/bug#6552 for an example of why `sym.owner.enclMethod hasAnnotation ScalaInlineClass`
          // is not suitable; if we make a method-local class non-private, it mangles outer pointer names.
          def enclMethodIsInline = closestEnclMethod(currentOwner) hasAnnotation ScalaInlineClass
          // scala/bug#8710 The extension method condition reflects our knowledge that a call to `new Meter(12).privateMethod`
          //         with later be rewritten (in erasure) to `Meter.privateMethod$extension(12)`.
          if ((currentClass != sym.owner || enclMethodIsInline) && !sym.isMethodWithExtension)
            sym.makeNotPrivate(sym.owner)

          val qsym = qual.tpe.widen.typeSymbol
          if (sym.isProtected && //(4)
              (qsym.isTrait || !(qual.isInstanceOf[Super] || (qsym isSubClass currentClass))))
            sym setFlag notPROTECTED
          super.transform(tree)

        case Apply(sel @ Select(qual, nme.CONSTRUCTOR), args) if isInner(sel.symbol.owner) =>
          val outerVal = atPos(tree.pos)(qual match {
            // it's a call between constructors of same class
            case _: This  =>
              assert(outerParam != NoSymbol, tree)
              outerValue
            case _        =>
              gen.mkAttributedQualifier(qual.tpe.prefix match {
                case NoPrefix => sym.owner.outerClass.thisType
                case x        => x
              })
          })
          super.transform(treeCopy.Apply(tree, sel, outerVal :: args))

        // for the pattern matcher
        // base.<outer>.eq(o) --> base.$outer().eq(o) if there's an accessor, else the whole tree becomes TRUE
        // TODO remove the synthetic `<outer>` method from outerFor??
        case Apply(eqsel@Select(Apply(sel@Select(base, nme.OUTER_SYNTH), Nil), eq), args) =>
          val outerFor = sel.symbol.owner
          val acc = outerAccessor(outerFor)

          if (acc == NoSymbol ||
              // since we can't fix scala/bug#4440 properly (we must drop the outer accessors of final classes when there's no immediate reference to them in sight)
              // at least don't crash... this duplicates maybeOmittable from constructors
              (acc.owner.isEffectivelyFinal && !acc.isOverridingSymbol)) {
            if (!base.tpe.hasAnnotation(UncheckedClass))
              runReporting.warning(tree.pos, "The outer reference in this type test cannot be checked at run time.", WarningCategory.Unchecked, currentOwner)
            transform(TRUE) // urgh... drop condition if there's no accessor (or if it may disappear after constructors)
          } else {
            // println("(base, acc)= "+(base, acc))
            val outerSelect = localTyper typed Apply(Select(base, acc), Nil)
            // achieves the same as: localTyper typed atPos(tree.pos)(outerPath(base, base.tpe.typeSymbol, outerFor.outerClass))
            // println("(b, tpsym, outerForI, outerFor, outerClass)= "+ (base, base.tpe.typeSymbol, outerFor, sel.symbol.owner, outerFor.outerClass))
            // println("outerSelect = "+ outerSelect)
            transform(treeCopy.Apply(tree, treeCopy.Select(eqsel, outerSelect, eq), args))
          }

        // (t12312) C.this.a().X().isInstanceOf[C.this.a.X.type]() -->
        // D.this.$outer().a().X().isInstanceOf[D.this.$outer.a.X.type]()
        case TypeApply(fun, targs) =>
          val rewriteTypeToExplicitOuter = new TypeMap { typeMap =>
            def apply(tp: Type) = tp match {
              case ThisType(sym) if sym != currentClass && !(sym.hasModuleFlag && sym.isStatic) =>
                var cls = currentClass
                var tpe = cls.thisType
                do {
                  tpe = singleType(tpe, outerAccessor(cls))
                  cls = cls.outerClass
                } while (cls != NoSymbol && sym != cls)
                tpe.mapOver(typeMap)
              case tp => tp.mapOver(typeMap)
            }
          }
          val fun2   = transform(fun)
          val targs2 = targs.mapConserve { targ0 =>
            val targ    = transform(targ0)
            val targTp  = targ.tpe
            val targTp2 = rewriteTypeToExplicitOuter(targTp.dealias)
            if (targTp eq targTp2) targ else TypeTree(targTp2).setOriginal(targ)
          }
          treeCopy.TypeApply(tree, fun2, targs2)

        case _ =>
          val x = super.transform(tree)
          if (x.tpe eq null) x
          else x setType transformInfo(currentOwner, x.tpe)
      }
    }

    /** The transformation method for whole compilation units */
    override def transformUnit(unit: CompilationUnit): Unit = {
      exitingExplicitOuter(super.transformUnit(unit))
    }
  }

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = new OuterPhase(prev)

  @nowarn("""cat=deprecation&origin=scala\.tools\.nsc\.transform\.ExplicitOuter\.Phase""")
  final type OuterPhase = Phase

  @nowarn("msg=shadowing a nested class of a parent is deprecated")
  @deprecated("use OuterPhase instead", since = "2.13.4")
  class Phase(prev: scala.tools.nsc.Phase) extends InfoPhase(prev) {
    override val checkable = false
  }
}
