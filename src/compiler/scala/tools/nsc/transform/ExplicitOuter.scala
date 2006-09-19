/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.transform

import symtab._
import Flags._
import scala.collection.mutable.{HashMap, ListBuffer}
import matching.{TransMatcher, PatternNodes, CodeFactory, PatternMatchers}

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class ExplicitOuter extends InfoTransform with TransMatcher with PatternNodes with CodeFactory with PatternMatchers with TypingTransformers {
  import global._
  import definitions._
  import posAssigner.atPos

  /** The following flags may be set by this phase: */
  override def phaseNewFlags: long = notPRIVATE | notPROTECTED

  /** the name of the phase: */
  val phaseName: String = "explicitouter"

  /** This class does not change linearization */
  override def changesBaseClasses = false

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ExplicitOuterTransformer(unit)

  /** Is given `clazz' an inner class? */
  private def isInner(clazz: Symbol) =
    !clazz.isPackageClass && !clazz.outerClass.isStaticOwner

  private def outerField(clazz: Symbol): Symbol = {
    val result = clazz.info.member(nme.getterToLocal(nme.OUTER))
    if (result == NoSymbol)
      assert(false, "no outer field in "+clazz+clazz.info.decls+" at "+phase)
    result
  }

  def outerAccessor(clazz: Symbol): Symbol = {
    val firstTry = clazz.info.decl(clazz.expandedName(nme.OUTER))
    if (firstTry != NoSymbol && firstTry.outerSource == clazz) firstTry
    else {
      var e = clazz.info.decls.elems
      while (e != null && e.sym.outerSource != clazz) e = e.next
      if (e != null) e.sym else NoSymbol
    }
  }

  /** The type transformation method:
   *  1. Add an outer parameter to the formal parameters of a constructor
   *     in a inner non-trait class;
   *  2. Add a protected $outer field to an inner class which is not a trait.
   *  3. Add an outer accessor $outer$$C to every inner class with fully qualified name C
   *     that is not an interface. The outer accesssor is abstract for traits, concrete
   *     for other classes.
   *  3a. Also add overriding accessor defs to every class that inherits mixin classes
   *     with outer accessor defs (unless the superclass already inherits the same mixin).
   *  4. Add a mixin constructor $init$ to all mixins except interfaces
   *     Leave all other types unchanged. todo: move to later
   *  5. Make all super accessors and modules in traits non-private, mangling their names.
   *  6. Remove protected flag from all members of traits.
   */
  def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case MethodType(formals, restpe) =>
      if (sym.owner.isTrait && ((sym hasFlag SUPERACCESSOR) || sym.isModule)) { // 5
        sym.makeNotPrivate(sym.owner)
      }
      if (sym.owner.isTrait && (sym hasFlag PROTECTED)) sym setFlag notPROTECTED // 6
      if (sym.isClassConstructor && isInner(sym.owner)) // 1
        MethodType(sym.owner.outerClass.thisType :: formals, restpe)
      else tp
    case ClassInfoType(parents, decls, clazz) =>
      var decls1 = decls
      if (isInner(clazz) && !(clazz hasFlag INTERFACE)) {
        decls1 = newScope(decls.toList)
        val outerAcc = clazz.newMethod(clazz.pos, nme.OUTER) // 3
        outerAcc.expandName(clazz)
        val restpe = if (clazz.isTrait) clazz.outerClass.tpe else clazz.outerClass.thisType
        decls1 enter (
          clazz.newOuterAccessor(clazz.pos)
          setInfo MethodType(List(), restpe))
        if (!clazz.isTrait) // 2
          //todo: avoid outer field if superclass has same outer value?
          decls1 enter (
            clazz.newValue(clazz.pos, nme.getterToLocal(nme.OUTER))
            setFlag (PROTECTED | PARAMACCESSOR)
            setInfo clazz.outerClass.thisType)
      }
      if (!parents.isEmpty) {
        for (val mc <- clazz.mixinClasses) {
          val mixinOuterAcc: Symbol = atPhase(phase.next)(outerAccessor(mc))
          if (mixinOuterAcc != NoSymbol) {
            if (decls1 eq decls) decls1 = newScope(decls.toList)
            val newAcc = mixinOuterAcc.cloneSymbol(clazz)
            newAcc.resetFlag(DEFERRED)
            newAcc.setInfo(clazz.thisType.memberType(mixinOuterAcc))
            decls1 enter newAcc
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

  /** A base class for transformers that maintain `outerParam' values for
   *  outer parameters of constructors.
   *  The class provides methods for referencing via outer.
   */
  abstract class OuterPathTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    /** The directly enclosing outer parameter, if we are in a constructor */
    protected var outerParam: Symbol = NoSymbol

    /** The first outer selection from currently transformed tree.
     *  The result is typed but not positioned.
     */
    protected def outerValue: Tree =
      if (outerParam != NoSymbol) gen.mkAttributedIdent(outerParam)
      else outerSelect(gen.mkAttributedThis(currentClass))

    /** Select and apply outer accessor from 'base'
     *  The result is typed but not positioned.
     */
    private def outerSelect(base: Tree): Tree =
      localTyper.typed(Apply(Select(base, outerAccessor(base.tpe.symbol)), List()))

    /** The path
     *     `base'.$outer$$C1 ... .$outer$$Cn
     *  which refers to the outer instance of class <code>to</code> of
     *  value <code>base</code>. The result is typed but not positioned.
     *
     *  @param base ...
     *  @param from ...
     *  @param to   ...
     *  @return     ...
     */
    protected def outerPath(base: Tree, from: Symbol, to: Symbol): Tree = {
      //Console.println("outerPath from "+from+" to "+to+" at "+base+":"+base.tpe)
      assert(base.tpe.baseType(from.toInterface) != NoType, base.tpe)
      if (from == to || from.isImplClass && from.toInterface == to) base
      else outerPath(outerSelect(base), from.outerClass, to)
    }

    override def transform(tree: Tree): Tree = {
      val savedOuterParam = outerParam
      try {
        tree match {
          case Template(_, _) =>
            outerParam = NoSymbol
          case DefDef(_, _, _, vparamss, _, _) =>
            if (tree.symbol.isClassConstructor && isInner(tree.symbol.owner)) {
              outerParam = vparamss.head.head.symbol
              assert(outerParam.name == nme.OUTER)
            }
          case _ =>
        }
        super.transform(tree)
      } catch {//debug
        case ex: Throwable =>
          System.out.println("exception when transforming " + tree)
          throw ex
      } finally {
        outerParam = savedOuterParam
      }
    }
  }

  /** The phase performs the following transformations on terms:
   *   1. An class which is not an interface and is not static gets an outer accessor
   *      (@see outerDefs)
   *   1a. A class which is not a trait gets an outer field.
   *   2. A mixin which is not also an interface gets a mixin constructor
   *      (@see mixinConstructorDef)
   *   3. Constructor bodies are augmented by calls to supermixin constructors
   *      (@see addMixinConstructorCalls)
   *   4. A constructor of a non-trait inner class gets an outer parameter.
   *   5. A reference C.this where C refers to an outer class is replaced by a selection
   *        this.$outer$$C1 ... .$outer$$Cn (@see outerPath)
   *   7. A call to a constructor Q.<init>(args) or Q.$init$(args) where Q != this and
   *      the constructor belongs to a non-static class is augmented by an outer argument.
   *      E.g. Q.<init>(OUTER, args) where OUTER is the qualifier corresponding to the
   *      singleton type Q.
   *   8. A call to a constructor this.<init>(args) in a secondary constructor
   *      is augmented to this.<init>(OUTER, args) where OUTER is the last parameter
   *      of the secondary constructor.
   *   9. Remove  `private' modifier from class members M that are accessed from an inner class.
   *   10.Remove `protected' modifier from class members M that are accessed
   *      without a super qualifier accessed from an inner class or trait.
   *   11. Remove `private' and `protected' modifiers from type symbols
   *   12. Remove `private' modifiers from members of traits
   *   Note: The whole transform is run in phase explicitOuter.next
   */
  class ExplicitOuterTransformer(unit: CompilationUnit) extends OuterPathTransformer(unit) {

    /** The definition tree of the outer accessor of current class
     */
    def outerFieldDef: Tree = {
      val outerF = outerField(currentClass)
      ValDef(outerF, EmptyTree)
    }

    /** The definition tree of the outer accessor of current class
     */
    def outerAccessorDef: Tree = {
      val outerAcc = outerAccessor(currentClass)
      var rhs = if (outerAcc hasFlag DEFERRED) EmptyTree
                else Select(This(currentClass), outerField(currentClass))
      localTyper.typed {
        atPos(currentClass.pos) {
          DefDef(outerAcc, vparamss => rhs)
        }
      }
    }

    /** The definition tree of the outer accessor for class `mixinClass'
     *  @param mixinClass The mixin class which defines the abstract outer accessor which is
     *                    implemented by the generated one.
     *  @pre mixinClass is an inner class
     */
    def mixinOuterAccessorDef(mixinClass: Symbol): Tree = {
      val outerAcc = outerAccessor(mixinClass).overridingSymbol(currentClass)
      if (outerAcc == NoSymbol)
        Console.println("cc " + currentClass + ":" + currentClass.info.decls +
                        " at " + phase)//debug
      assert(outerAcc != NoSymbol)
      val path = gen.mkAttributedQualifier(currentClass.thisType.baseType(mixinClass).prefix)
      val rhs = ExplicitOuterTransformer.this.transform(path)
      localTyper.typed {
        atPos(currentClass.pos) {
          DefDef(outerAcc, vparamss => rhs)
        }
      }
    }

    /** The main transformation method */
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      if (sym != null && sym.isType) {//(9)
        if (sym hasFlag PRIVATE) sym setFlag notPRIVATE
        if (sym hasFlag PROTECTED) sym setFlag notPROTECTED
      }
      tree match {
        case Template(parents, decls) =>
          val newDefs = new ListBuffer[Tree]
          atOwner(tree, currentOwner) {
            if (!(currentClass hasFlag INTERFACE) || (currentClass hasFlag lateINTERFACE)) {
              if (isInner(currentClass)) {
                if (!currentClass.isTrait) newDefs += outerFieldDef // (1a)
                newDefs += outerAccessorDef // (1)
              }
              if (!currentClass.isTrait)
                for (val mc <- currentClass.mixinClasses)
                  if (outerAccessor(mc) != NoSymbol)
                    newDefs += mixinOuterAccessorDef(mc)
            }
          }
          super.transform(
            copy.Template(tree, parents, if (newDefs.isEmpty) decls else decls ::: newDefs.toList))

        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          if (sym.isClassConstructor) {
            rhs match {
              case Literal(_) =>
                // replace unit rhs () by empty block {()}
                val rhs1 = Block(List(), rhs) setPos rhs.pos setType rhs.tpe
                transform(copy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs1))
              case _ =>
                val clazz = sym.owner
                val vparamss1 =
                  if (isInner(clazz)) { // (4)
                    val outerParam =
                      sym.newValueParameter(sym.pos, nme.OUTER) setInfo outerField(clazz).info
                    ((ValDef(outerParam) setType NoType) :: vparamss.head) :: vparamss.tail
                  } else vparamss
                super.transform(copy.DefDef(tree, mods, name, tparams, vparamss1, tpt, rhs))
            }
          } else { //todo: see whether we can move this to transformInfo
            if (sym.owner.isTrait && (sym hasFlag (ACCESSOR | SUPERACCESSOR)))
              sym.makeNotPrivate(sym.owner); //(2)
            super.transform(tree)
          }

        case This(qual) =>
          if (sym == currentClass || (sym hasFlag MODULE) && sym.isStatic) tree
          else atPos(tree.pos)(outerPath(outerValue, currentClass.outerClass, sym)) // (5)

        case Select(qual, name) =>
          if (currentClass != sym.owner && currentClass != sym.moduleClass) // (3)
            sym.makeNotPrivate(sym.owner)
          val qsym = qual.tpe.widen.symbol
          if ((sym hasFlag PROTECTED) && //(4)
              (qsym.isTrait || !(qual.isInstanceOf[Super] || (qsym isSubClass currentClass))))
            sym setFlag notPROTECTED
          super.transform(tree)

        case Apply(sel @ Select(qual, name), args)
          if (name == nme.CONSTRUCTOR && isInner(sel.symbol.owner)) =>
            val outerVal = atPos(tree.pos) {
              if (qual.isInstanceOf[This]) { // it's a call between constructors of same class
                assert(outerParam != NoSymbol)
                outerValue
              } else {
                var pre = qual.tpe.prefix
                if (pre == NoPrefix) pre = sym.owner.outerClass.thisType
                gen.mkAttributedQualifier(pre)
              }
            }
            super.transform(copy.Apply(tree, sel, outerVal :: args))

        case Match(selector, cases) => // <----- transmatch hook
          val tid = if (settings.debug.value) {
            val q = unit.fresh.newName("tidmark")
            Console.println("transforming patmat with tidmark "+q+" ncases = "+cases.length)
            q
          } else null
          if ((cases.length > 1) && (treeInfo.isDefaultCase(cases(0))))
            assert(false,"transforming too much, " + tid)

          val nselector = transform(selector)
          assert(nselector.tpe =:= selector.tpe)
          val ncases = transformCaseDefs(cases)

          ExplicitOuter.this.resultType = tree.tpe
          //Console.println("TransMatcher currentOwner ="+currentOwner+")")
          //Console.println("TransMatcher selector.tpe ="+selector.tpe+")")
          //Console.println("TransMatcher resultType ="+resultType+")")

          val t_untyped = handlePattern(nselector, ncases, currentOwner, transform)
          //Console.println("t_untyped "+t_untyped.toString())
          val t = atPos(tree.pos) { localTyper.typed(t_untyped, resultType) }

          //t = transform(t)
          //val t         = atPos(tree.pos) { typed(t_untyped, resultType) }
          //val t         = atPos(tree.pos) { typed(t_untyped) }
          //Console.println("t typed "+t.toString())
          if (settings.debug.value)
            Console.println("finished translation of " + tid)
          t

        case _ =>
          super.transform(tree)
      }
    }

    /** The transformation method for whole compilation units */
    override def transformUnit(unit: CompilationUnit): unit = {
      cunit = unit
      atPhase(phase.next) { super.transformUnit(unit) }
    }
  }
}


