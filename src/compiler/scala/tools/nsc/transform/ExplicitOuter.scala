/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags.{ CASE => _, _ }
import scala.collection.mutable.ListBuffer
import matching.{ Patterns, ParallelMatching }

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class ExplicitOuter extends InfoTransform
      with Patterns
      with ParallelMatching
      with TypingTransformers
      with ast.TreeDSL
{
  import global._
  import definitions._
  import CODE._
  import Debug.TRACE

  /** The following flags may be set by this phase: */
  override def phaseNewFlags: Long = notPROTECTED

  /** the name of the phase: */
  val phaseName: String = "explicitouter"

  /** This class does not change linearization */
  override def changesBaseClasses = false

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ExplicitOuterTransformer(unit)

  /** Is given clazz an inner class? */
  private def isInner(clazz: Symbol) =
    !clazz.isPackageClass && !clazz.outerClass.isStaticOwner

  private def haveSameOuter(parent: Type, clazz: Symbol) = parent match {
    case TypeRef(pre, sym, _)   =>
      val owner = clazz.owner

      sym.isClass && owner.isClass &&
      owner == sym.owner &&
      owner.thisType =:= pre
    case _                      => false
  }

  /** Does given clazz define an outer field? */
  def hasOuterField(clazz: Symbol) = {
    val parents = clazz.info.parents

    isInner(clazz) && !clazz.isTrait && {
      parents.isEmpty || !haveSameOuter(parents.head, clazz)
    }
  }

  private def outerField(clazz: Symbol): Symbol = {
    val result = clazz.info.member(nme.OUTER_LOCAL)
    assert(result != NoSymbol, "no outer field in "+clazz+" at "+phase)

    result
  }

  private val innerClassConstructorParamName: TermName = newTermName("arg" + nme.OUTER)

  class RemoveBindingsTransformer(toRemove: Set[Symbol]) extends Transformer {
    override def transform(tree: Tree) = tree match {
      case Bind(_, body) if toRemove(tree.symbol) =>
        TRACE("Dropping unused binding: " + tree.symbol)
        super.transform(body)
      case _                                      => super.transform(tree)
    }
  }

  /** Issue a migration warning for instance checks which might be on an Array and
   *  for which the type parameter conforms to Seq, because these answers changed in 2.8.
   */
  def isArraySeqTest(lhs: Type, rhs: Type) =
    (ArrayClass.tpe <:< lhs.widen) && (rhs.widen matchesPattern SeqClass.tpe)

  def outerAccessor(clazz: Symbol): Symbol = {
    val firstTry = clazz.info.decl(nme.expandedName(nme.OUTER, clazz))
    if (firstTry != NoSymbol && firstTry.outerSource == clazz) firstTry
    else findOrElse(clazz.info.decls)(_.outerSource == clazz)(NoSymbol)
  }
  def newOuterAccessor(clazz: Symbol) = {
    val accFlags = SYNTHETIC | METHOD | STABLE | ( if (clazz.isTrait) DEFERRED else 0 )
    val sym      = clazz.newMethod(nme.OUTER, clazz.pos, accFlags)
    val restpe   = if (clazz.isTrait) clazz.outerClass.tpe else clazz.outerClass.thisType

    sym expandName clazz
    sym.referenced = clazz
    sym setInfo MethodType(Nil, restpe)
  }
  def newOuterField(clazz: Symbol) = {
    val accFlags = SYNTHETIC | PARAMACCESSOR | ( if (clazz.isEffectivelyFinal) PrivateLocal else PROTECTED )
    val sym      = clazz.newValue(nme.OUTER_LOCAL, clazz.pos, accFlags)

    sym setInfo clazz.outerClass.thisType
  }

  /** <p>
   *    The type transformation method:
   *  </p>
   *  <ol>
   *    <li>
   *      Add an outer parameter to the formal parameters of a constructor
   *      in a inner non-trait class;
   *    </li>
   *    <li>
   *      Add a protected $outer field to an inner class which is
   *      not a trait.
   *    </li>
   *    <li>
   *      <p>
   *        Add an outer accessor $outer$$C to every inner class
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
    case MethodType(params, restpe1) =>
      val restpe = transformInfo(sym, restpe1)
      if (sym.owner.isTrait && ((sym hasFlag (ACCESSOR | SUPERACCESSOR)) || sym.isModule)) { // 5
        sym.makeNotPrivate(sym.owner)
      }
      if (sym.owner.isTrait && sym.isProtected) sym setFlag notPROTECTED // 6
      if (sym.isClassConstructor && isInner(sym.owner)) { // 1
        val p = sym.newValueParameter(innerClassConstructorParamName, sym.pos)
                   .setInfo(sym.owner.outerClass.thisType)
        MethodType(p :: params, restpe)
      } else if (restpe ne restpe1)
        MethodType(params, restpe)
      else tp
    case ClassInfoType(parents, decls, clazz) =>
      var decls1 = decls
      if (isInner(clazz) && !clazz.isInterface) {
        decls1 = decls.cloneScope
        val outerAcc = clazz.newMethod(nme.OUTER, clazz.pos) // 3
        outerAcc expandName clazz

        decls1 enter newOuterAccessor(clazz)
        if (hasOuterField(clazz)) //2
          decls1 enter newOuterField(clazz)
      }
      if (!clazz.isTrait && !parents.isEmpty) {
        for (mc <- clazz.mixinClasses) {
          val mixinOuterAcc: Symbol = afterExplicitOuter(outerAccessor(mc))
          if (mixinOuterAcc != NoSymbol) {
            if (decls1 eq decls) decls1 = decls.cloneScope
            val newAcc = mixinOuterAcc.cloneSymbol(clazz, mixinOuterAcc.flags & ~DEFERRED)
            newAcc setInfo (clazz.thisType memberType mixinOuterAcc)
            decls1 enter newAcc
          }
        }
      }
      if (decls1 eq decls) tp else ClassInfoType(parents, decls1, clazz)
    case PolyType(tparams, restp) =>
      val restp1 = transformInfo(sym, restp)
      if (restp eq restp1) tp else PolyType(tparams, restp1)

    case _ =>
      // Local fields of traits need to be unconditionally unprivatized.
      // Reason: Those fields might need to be unprivatized if referenced by an inner class.
      // On the other hand, mixing in the trait into a separately compiled
      // class needs to have a common naming scheme, independently of whether
      // the field was accessed from an inner class or not. See #2946
      if (sym.owner.isTrait && sym.hasLocalFlag &&
              (sym.getter(sym.owner.toInterface) == NoSymbol))
        sym.makeNotPrivate(sym.owner)
      tp
  }

  /** A base class for transformers that maintain outerParam
   *  values for outer parameters of constructors.
   *  The class provides methods for referencing via outer.
   */
  abstract class OuterPathTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    /** The directly enclosing outer parameter, if we are in a constructor */
    protected var outerParam: Symbol = NoSymbol

    /** The first outer selection from currently transformed tree.
     *  The result is typed but not positioned.
     */
    protected def outerValue: Tree =
      if (outerParam != NoSymbol) ID(outerParam)
      else outerSelect(THIS(currentClass))

    /** Select and apply outer accessor from 'base'
     *  The result is typed but not positioned.
     *  If the outer access is from current class and current class is final
     *  take outer field instead of accessor
     */
    private def outerSelect(base: Tree): Tree = {
      val outerAcc = outerAccessor(base.tpe.typeSymbol.toInterface)
      val currentClass = this.currentClass //todo: !!! if this line is removed, we get a build failure that protected$currentClass need an override modifier
      // outerFld is the $outer field of the current class, if the reference can
      // use it (i.e. reference is allowed to be of the form this.$outer),
      // otherwise it is NoSymbol
      val outerFld =
        if (outerAcc.owner == currentClass &&
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

    /** The path
     *  <blockquote><pre>`base'.$outer$$C1 ... .$outer$$Cn</pre></blockquote>
     *  which refers to the outer instance of class to of
     *  value base. The result is typed but not positioned.
     *
     *  @param base ...
     *  @param from ...
     *  @param to   ...
     *  @return     ...
     */
    protected def outerPath(base: Tree, from: Symbol, to: Symbol): Tree = {
      //Console.println("outerPath from "+from+" to "+to+" at "+base+":"+base.tpe)
      //assert(base.tpe.widen.baseType(from.toInterface) != NoType, ""+base.tpe.widen+" "+from.toInterface)//DEBUG
      if (from == to || from.isImplClass && from.toInterface == to) base
      else outerPath(outerSelect(base), from.outerClass, to)
    }

    override def transform(tree: Tree): Tree = {
      val savedOuterParam = outerParam
      try {
        tree match {
          case Template(_, _, _) =>
            outerParam = NoSymbol
          case DefDef(_, _, _, vparamss, _, _) =>
            if (tree.symbol.isClassConstructor && isInner(tree.symbol.owner)) {
              outerParam = vparamss.head.head.symbol
              assert(outerParam.name startsWith nme.OUTER, outerParam.name)
            }
          case _ =>
        }
        super.transform(tree)
      }
      finally outerParam = savedOuterParam
    }
  }

  /** <p>
   *    The phase performs the following transformations on terms:
   *  </p>
   *  <ol>
   *    <li> <!-- 1 -->
   *      <p>
   *        An class which is not an interface and is not static gets an outer
   *        accessor (@see outerDefs).
   *      </p>
   *      <p>
   *        1a. A class which is not a trait gets an outer field.
   *      </p>
   *    </li>
   *    <li> <!-- 4 -->
   *      A constructor of a non-trait inner class gets an outer parameter.
   *    </li>
   *    <li> <!-- 5 -->
   *      A reference C.this where C refers to an
   *      outer class is replaced by a selection
   *      this.$outer$$C1 ... .$outer$$Cn (@see outerPath)
   *    </li>
   *    <li>
   *    </li>
   *    <li> <!-- 7 -->
   *      A call to a constructor Q.<init>(args) or Q.$init$(args) where Q != this and
   *      the constructor belongs to a non-static class is augmented by an outer argument.
   *      E.g. Q.<init>(OUTER, args) where OUTER
   *      is the qualifier corresponding to the singleton type Q.
   *    </li>
   *    <li>
   *      A call to a constructor this.<init>(args) in a
   *      secondary constructor is augmented to this.<init>(OUTER, args)
   *      where OUTER is the last parameter of the secondary constructor.
   *    </li>
   *    <li> <!-- 9 -->
   *      Remove private modifier from class members M
   *      that are accessed from an inner class.
   *    </li>
   *    <li> <!-- 10 -->
   *      Remove protected modifier from class members M
   *      that are accessed without a super qualifier accessed from an inner
   *      class or trait.
   *    </li>
   *    <li> <!-- 11 -->
   *      Remove private and protected modifiers
   *      from type symbols
   *    </li>
   *    <li> <!-- 12 -->
   *      Remove private modifiers from members of traits
   *    </li>
   *  </ol>
   *  <p>
   *    Note: The whole transform is run in phase explicitOuter.next.
   *  </p>
   */
  class ExplicitOuterTransformer(unit: CompilationUnit) extends OuterPathTransformer(unit) {
    transformer =>

    /** The definition tree of the outer accessor of current class
     */
    def outerFieldDef: Tree = VAL(outerField(currentClass)) === EmptyTree

    /** The definition tree of the outer accessor of current class
     */
    def outerAccessorDef: Tree = {
      val outerAcc = outerAccessor(currentClass)
      var rhs: Tree =
        if (outerAcc.isDeferred) EmptyTree
        else This(currentClass) DOT outerField(currentClass)

      /** If we don't re-type the tree, we see self-type related crashes like #266.
       */
      localTyper typed {
        (DEF(outerAcc) withPos currentClass.pos withType null) === rhs
      }
    }

    /** The definition tree of the outer accessor for class mixinClass.
     *
     *  @param mixinClass The mixin class which defines the abstract outer
     *                    accessor which is implemented by the generated one.
     *  @pre mixinClass is an inner class
     */
    def mixinOuterAccessorDef(mixinClass: Symbol): Tree = {
      val outerAcc    = outerAccessor(mixinClass) overridingSymbol currentClass
      def mixinPrefix = (currentClass.thisType baseType mixinClass).prefix
      assert(outerAcc != NoSymbol, "No outer accessor for inner mixin " + mixinClass + " in " + currentClass)
      // I added the mixinPrefix.typeArgs.nonEmpty condition to address the
      // crash in SI-4970.  I feel quite sure this can be improved.
      val path = (
        if (mixinClass.owner.isTerm) gen.mkAttributedThis(mixinClass.owner.enclClass)
        else if (mixinPrefix.typeArgs.nonEmpty) gen.mkAttributedThis(mixinPrefix.typeSymbol)
        else gen.mkAttributedQualifier(mixinPrefix)
      )
      localTyper typed {
        (DEF(outerAcc) withPos currentClass.pos) === {
          // Need to cast for nested outer refs in presence of self-types. See ticket #3274.
          gen.mkCast(transformer.transform(path), outerAcc.info.resultType)
        }
      }
    }

    def matchTranslation(tree: Match) = {
      val Match(selector, cases) = tree
      var nselector = transform(selector)

      def makeGuardDef(vs: List[Symbol], guard: Tree) = {
        val gdname = unit.freshTermName("gd")
        val method = currentOwner.newMethod(gdname, tree.pos, SYNTHETIC)
        val params = method newSyntheticValueParams vs.map(_.tpe)
        method setInfo new MethodType(params, BooleanClass.tpe)

        localTyper typed {
          DEF(method) === guard.changeOwner(currentOwner -> method).substituteSymbols(vs, params)
        }
      }

      val nguard = new ListBuffer[Tree]
      val ncases =
        for (CaseDef(pat, guard, body) <- cases) yield {
          // Strip out any unused pattern bindings up front
          val patternIdents = for (b @ Bind(_, _) <- pat) yield b.symbol
          val references: Set[Symbol] = Set(guard, body) flatMap { t => for (id @ Ident(name) <- t) yield id.symbol }
          val (used, unused) = patternIdents partition references
          val strippedPat = if (unused.isEmpty) pat else new RemoveBindingsTransformer(unused.toSet) transform pat

          val gdcall =
            if (guard == EmptyTree) EmptyTree
            else {
              val guardDef = makeGuardDef(used, guard)
              nguard += transform(guardDef) // building up list of guards

              localTyper typed (Ident(guardDef.symbol) APPLY (used map Ident))
            }

          (CASE(transform(strippedPat)) IF gdcall) ==> transform(body)
        }

      val (checkExhaustive, requireSwitch) = nselector match {
        case Typed(nselector1, tpt) =>
          val unchecked = treeInfo.isUncheckedAnnotation(tpt.tpe)
          if (unchecked)
            nselector = nselector1

          // Don't require a tableswitch if there are 1-2 casedefs
          // since the matcher intentionally emits an if-then-else.
          (!unchecked, treeInfo.isSwitchAnnotation(tpt.tpe) && ncases.size > 2)
        case _  =>
          (true, false)
      }

      val t = atPos(tree.pos) {
        val context     = MatrixContext(currentUnit, transform, localTyper, currentOwner, tree.tpe)
        val t_untyped   = handlePattern(nselector, ncases, checkExhaustive, context)

        /* if @switch annotation is present, verify the resulting tree is a Match */
        if (requireSwitch) t_untyped match {
          case Block(_, Match(_, _))  => // ok
          case _                      =>
            unit.error(tree.pos, "could not emit switch for @switch annotated match")
        }

        localTyper.typed(t_untyped, context.matchResultType)
      }

      if (nguard.isEmpty) t
      else Block(nguard.toList, t) setType t.tpe
    }

    /** The main transformation method */
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      if (sym != null && sym.isType) { //(9)
        if (sym.isPrivate) sym setFlag notPRIVATE
        if (sym.isProtected) sym setFlag notPROTECTED
      }
      tree match {
        case Template(parents, self, decls) =>
          val newDefs = new ListBuffer[Tree]
          atOwner(tree, currentOwner) {
            if (!currentClass.isInterface || (currentClass hasFlag lateINTERFACE)) {
              if (isInner(currentClass)) {
                if (hasOuterField(currentClass))
                  newDefs += outerFieldDef // (1a)
                newDefs += outerAccessorDef // (1)
              }
              if (!currentClass.isTrait)
                for (mc <- currentClass.mixinClasses)
                  if (outerAccessor(mc) != NoSymbol)
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
            rhs match {
              case Literal(_) =>
                sys.error("unexpected case") //todo: remove
              case _ =>
                val clazz = sym.owner
                val vparamss1 =
                  if (isInner(clazz)) { // (4)
                    val outerParam =
                      sym.newValueParameter(nme.OUTER, sym.pos) setInfo outerField(clazz).info
                    ((ValDef(outerParam) setType NoType) :: vparamss.head) :: vparamss.tail
                  } else vparamss
                super.transform(copyDefDef(tree)(vparamss = vparamss1))
            }
          } else
            super.transform(tree)

        case This(qual) =>
          if (sym == currentClass || sym.hasModuleFlag && sym.isStatic) tree
          else atPos(tree.pos)(outerPath(outerValue, currentClass.outerClass, sym)) // (5)

        case Select(qual, name) =>
          if (currentClass != sym.owner) // (3)
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

        // entry point for pattern matcher translation
        case mch: Match if (!opt.virtPatmat) => // don't use old pattern matcher as fallback when the user wants the virtualizing one
          matchTranslation(mch)

        case _ =>
          if (opt.virtPatmat) { // this turned out to be expensive, hence the hacky `if` and `return`
            tree match {
              // for patmatvirtualiser
              // base.<outer>.eq(o) --> base.$outer().eq(o) if there's an accessor, else the whole tree becomes TRUE
              // TODO remove the synthetic `<outer>` method from outerFor??
              case Apply(eqsel@Select(eqapp@Apply(sel@Select(base, outerAcc), Nil), eq), args)  if outerAcc == nme.OUTER_SYNTH =>
                val outerFor = sel.symbol.owner.toInterface // TODO: toInterface necessary?
                val acc = outerAccessor(outerFor)
                if(acc == NoSymbol) {
                  // println("WARNING: no outer for "+ outerFor)
                  return transform(TRUE) // urgh... drop condition if there's no accessor
                } else {
                  // println("(base, acc)= "+(base, acc))
                  val outerSelect = localTyper typed Apply(Select(base, acc), Nil)
                  // achieves the same as: localTyper typed atPos(tree.pos)(outerPath(base, base.tpe.typeSymbol, outerFor.outerClass))
                  // println("(b, tpsym, outerForI, outerFor, outerClass)= "+ (base, base.tpe.typeSymbol, outerFor, sel.symbol.owner, outerFor.outerClass))
                  // println("outerSelect = "+ outerSelect)
                  return transform(treeCopy.Apply(tree, treeCopy.Select(eqsel, outerSelect, eq), args))
                }
              case _ =>
            }
          }

          if (settings.Xmigration28.value) tree match {
            case TypeApply(fn @ Select(qual, _), args) if fn.symbol == Object_isInstanceOf || fn.symbol == Any_isInstanceOf =>
              if (isArraySeqTest(qual.tpe, args.head.tpe))
                unit.warning(tree.pos, "An Array will no longer match as Seq[_].")
            case _ => ()
          }

          val x = super.transform(tree)
          if (x.tpe eq null) x
          else x setType transformInfo(currentOwner, x.tpe)
      }
    }

    /** The transformation method for whole compilation units */
    override def transformUnit(unit: CompilationUnit) {
      afterExplicitOuter(super.transformUnit(unit))
    }
  }

  override def newPhase(prev: scala.tools.nsc.Phase): StdPhase =
    new Phase(prev)

  class Phase(prev: scala.tools.nsc.Phase) extends super.Phase(prev) {
    override val checkable = false
  }
}
