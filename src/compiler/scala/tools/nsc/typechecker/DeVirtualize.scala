/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: RefChecks.scala 13735 2008-01-18 17:18:58Z odersky $

package scala.tools.nsc.typechecker

import symtab.Flags._
import transform.{InfoTransform, TypingTransformers}
import scala.tools.nsc.util.{Position, NoPosition}
import scala.collection.mutable.ListBuffer

abstract class DeVirtualize extends InfoTransform with TypingTransformers {

  import global._
  import definitions._
  import typer.{typed, typedOperator, atOwner}
  import posAssigner.atPos

  /** the following two members override abstract members in Transform */
  val phaseName: String = "devirtualize"

  /** The phase might set the following new flags: */
  override def phaseNewFlags: Long = notOVERRIDE | notFINAL
  //
  // todo: this does not work yet: for some unknown reason the backend
  // generates unverifiable code when notOVERRIDE is set for any phase whatsoever
  // (I tried to set it later at phase Mixin, with same effect.
  // One gets error messages like the following:
  //
  // /home/odersky/scala/sabbus.xml:37: The following error occurred while executing this line:
  // /home/odersky/scala/sabbus.xml:456: Could not create type quick-bin due to java.lang.VerifyError: (class: scala/Option, method: productPrefix signature: ()Ljava/lang/String;) Illegal local variable number
  //
  // we need to fix this before notOVERRIDE can be turned on here.


  def newTransformer(unit: CompilationUnit): DeVirtualizeTransformer =
    new DeVirtualizeTransformer(unit)

  /** The class does not change base-classes of existing classes */
  override def changesBaseClasses = false

  def transformInfo(sym: Symbol, tp: Type): Type = devirtualizeMap(tp)

  /* todo:
     handle constructor arguments
     check: overriding classes must have same type params
     virtual classes cannot have self types
   */

  /** Do the following transformations everywhere in a type:
   *
   *  1. If a class defines virtual classes VC, add abstract types VA,
   *     worker traits VT and factories VF instead (@see devirtualize).
   *  2. For all virtual member classes VC which
   *     are not abstract and which are or inherit from a virtual class defined in current class
   *     add a factory (@see addFactory)
   *  3. Convert VC.this where VC is a virtual class to WT.this where WT is the worker trait for VC
   *     (@see workerTrait)
   *  4. Convert TypeRef's to VC where VC is a virtual class to TypeRef's to AT, where AT
   *     is the abstract type corresponding to VC.
   *
   *  Note: If a class inherits vc's from two different paths, a vc in the
   *  inheriting class has to be created beforehand. This is done in phase ??? (NOT YET DONE!)
   *
   *  Note: subclasses of virtual classes are treated as if they are virtual.
   *  isVirtualClass returns true for them also.
   */
  object devirtualizeMap extends TypeMap {
    def apply(tp: Type): Type = {
//      println("devirtualizeMap on " + tp)
      mapOver(tp) match {
      case tp1 @ ClassInfoType(parents, decls, clazz) if containsVirtuals(clazz) =>
//        println(clazz + " contains virtuals")
        transformOwnerInfo(clazz) // we might need to do this in two phases: enter/resolve
        val ds = decls.toList
        val decls1 = newScope(ds)
        for (m <- ds)
          if (m.isVirtualClass) devirtualize(m, decls1)
        for (m <- classesInNeedOfFactories(clazz))
          addFactory(m, clazz, decls1)
//        println("Built ourselves a " + ClassInfoType(parents, decls1, clazz))
        ClassInfoType(parents, decls1, clazz)
      case tp1 @ ThisType(clazz) if clazz.isVirtualClass =>
        ThisType(workerTrait(clazz))
      case tp1 @ TypeRef(pre, clazz, args) if clazz.isVirtualClass =>
        TypeRef(pre, abstractType(clazz), args)
      case tp1 =>
        tp1
      }
    }
  }

  /** Transform owner of given clazz symbol */
  protected def transformOwnerInfo(clazz: Symbol) { atPhase(ownPhase.next) { clazz.owner.info } }

  /** Names of derived classes and factories */
  protected def workerTraitName(clazzName: Name) = newTypeName(clazzName+"$trait")
  protected def concreteClassName(clazzName: Name) = newTypeName(clazzName+"$fix")
  protected def factoryName(clazzName: Name) = newTermName("new$"+clazzName)

  /** Does `clazz' contaion virtual classes? */
  protected def containsVirtuals(clazz: Symbol) = clazz.info.decls.toList exists (_.isVirtualClass)

  /** The inner classes that need factory methods in `clazz'
   *  This is intended to catch situations like the following
   *
   *  abstract class C {
   *    class V <: {...}
   *    class W extends V
   *  }
   *  class D extends C {
   *    class V <: {...}
   *    // factories needed for V and W!
   *  }
   */
  protected def classesInNeedOfFactories(clazz: Symbol) = atPhase(ownPhase) {
    def isDefinedVirtual(c: Symbol) = c.isVirtualClass && c.owner == clazz
    val buf = new ListBuffer[Symbol]
    for (m <- clazz.info.members)
      if (m.isVirtualClass && !(m hasFlag ABSTRACT) && (m.info.baseClasses exists isDefinedVirtual))
        buf += m
    buf.toList
  }

  /** The abstract type corresponding to a virtual class. */
  protected def abstractType(clazz: Symbol): Symbol = atPhase(ownPhase.next) {
//    println("Looking up the abstract type for " + clazz)
    val tsym = clazz.owner.info.member(clazz.name)
    assert(tsym.isAbstractType, clazz)
//    println("Found " + tsym)
    tsym
  }

  /** The worker trait corresponding to a virtual class. */
  protected def workerTrait(clazz: Symbol) = atPhase(ownPhase.next) {
    val tsym = clazz.owner.info.member(workerTraitName(clazz.name))
    assert(tsym.isTrait, clazz)
    tsym
  }

  /** The factory corresponding to a virtual class. */
  protected def factory(clazz: Symbol) = atPhase(ownPhase.next) {
    assert(!(clazz hasFlag ABSTRACT), clazz)
    val fsym = clazz.owner.info.member(factoryName(clazz.name))
    assert(fsym.isMethod, clazz)
    fsym
  }

  /** The flags that a worker trait can inherit from its virtual class */
  protected val traitFlagMask = AccessFlags

  /** The flags that an abstract type can inherit from its virtual class */
  protected val absTypeFlagMask = AccessFlags | DEFERRED

  /** The flags that a factory method can inherit from its virtual class */
  protected val factoryFlagMask = AccessFlags

  /** Create a polytype with given type parameters and given type, or return just the type
   *  if type params is empty. */
  protected def mkPolyType(tparams: List[Symbol], tp: Type) =
    if (tparams.isEmpty) tp else PolyType(tparams, tp)

  /** Set info of `dst' to `tp', potentially wrapped by copies of any type
   *  parameters of symbol `from' */
  def setPolyInfo(dst: Symbol, from: Symbol, tp: Type) = {
    val tparams = cloneSymbols(from.typeParams, dst)
    dst setInfo mkPolyType(tparams, tp substSym (from.typeParams, tparams))
  }

  /** Replace a virtual class
   *
   *  attrs mods class VC[Ts] <: Ps { decls }
   *
   * by the following symbols
   *
   *  attrs mods1 type VC[Ts] <: dvm(Ps) with VC$trait[Ts]
   *  attrs mods2 trait VC$trait[Ts] extends AnyRef with ScalaObject {
   *    this: VC[Ts] with VC$trait[Ts] => decls1
   *  }
   *
   * where
   *
   *  dvm    is the devirtalization mapping which converts refs to
   *         virtual classes to refs to their abstract types (@see devirtualize)
   *  mods1  are the modifiers inherited to abstract types
   *  mods2  are the modifiers inherited to worker traits
   *  decls1 is decls but members that have an override modifier
   *         lose it and any final modifier as well.
   */
  protected def devirtualize(clazz: Symbol, scope: Scope) {
    scope.unlink(clazz)

    val cabstype = clazz.owner.newAbstractType(clazz.pos, clazz.name)
      .setFlag(clazz.flags & absTypeFlagMask)
      .setAttributes(clazz.attributes)
    scope.enter(cabstype)

    cabstype setInfo new LazyType {
      override val typeParams = cloneSymbols(clazz.typeParams, cabstype)
      override def complete(sym: Symbol) {
        def parentTypeRef(tp: Type) =
          devirtualizeMap(tp.substSym(clazz.typeParams, typeParams))
        val parents = (clazz.info.parents map parentTypeRef) :::
          List(appliedType(workerTrait(clazz).typeConstructor, typeParams map (_.tpe)))
        sym.setInfo(
          mkPolyType(typeParams, mkTypeBounds(AllClass.tpe, intersectionType(parents))))
      }
    }

    val wtrait = clazz.owner.newClass(clazz.pos, workerTraitName(clazz.name))
      .setFlag(clazz.flags & traitFlagMask | TRAIT)
      .setAttributes(clazz.attributes)
    scope.enter(wtrait)

    // remove OVERRIDE from all workertrait members
    val decls1 = clazz.info.decls.toList
    for (val m <- decls1)
      if (m hasFlag OVERRIDE) m setFlag (notOVERRIDE | notFINAL)

    setPolyInfo(
      wtrait, clazz,
      ClassInfoType(List(ObjectClass.tpe, ScalaObjectClass.tpe), newScope(decls1), wtrait))
    wtrait.typeOfThis = intersectionType(List(
      appliedType(cabstype.typeConstructor, wtrait.typeParams map (_.tpe)),
      wtrait.tpe))
  }

  /* Add a factory symbol for a virtual class
   *
   *  attrs mods class VC[Ts] <: Ps { decls }
   *  with base classes BC[Us]'s
   *
   * which corresponds to the following definition :
   *
   *  attrs mods3 def new$VC[Ts](): VC[Ts] = {
   *    class VC$fix extends v2w(BC's[Ts]) with VC$trait[Ts] { ... }
   *    new VC$fix
   *  }
   *
   * where
   *
   *  mods3  are the modifiers inherited to factories
   *  v2w is maps every virtual class to its workertrait and leaves other types alone.
   *
   *  @param  clazz    The virtual class for which factory is added
   *  @param  owner    The owner for which factory is added as a member
   *  @param  scope    The scope into which factory is entered
   */
  def addFactory(clazz: Symbol, owner: Symbol, scope: Scope) {
//    println("Adding a factory to " + clazz.owner + "." + clazz)
    val pos = if (clazz.owner == owner) clazz.pos else owner.pos
    val factory = owner.newMethod(pos, factoryName(clazz.name))
      .setFlag(clazz.flags & factoryFlagMask)
      .setAttributes(clazz.attributes)
    scope.enter(factory)
    // val cabstype = abstractType(clazz)
//    setPolyInfo(factory, cabstype, MethodType(List(/*todo: handle constructor parameters*/),
//                                              cabstype.tpe))
    factory
  }

  /** The concrete class symbol VC$fix in the factory symbol (@see addFactory)
   *  @param clazz    the virtual class
   *  @param factory  the factory which returns an instance of this class
   */
  protected def concreteClassSym(clazz: Symbol, factory: Symbol) = {
    val cclazz = factory.newClass(clazz.pos, concreteClassName(clazz.name))
      .setFlag(FINAL)
      .setAttributes(clazz.attributes)

    cclazz setInfo new LazyType {
      override def complete(sym: Symbol) {
        def v2w(bc: Symbol): Type = {
          val btp = clazz.info baseType bc
          if (bc.isVirtualClass)
            (btp: @unchecked) match {
              case TypeRef(pre, _, args) =>
                TypeRef(pre, workerTrait(bc), args)
            }
          else btp
        }.substSym(clazz.typeParams, factory.typeParams)
        val parents = clazz.info.baseClasses.reverse map v2w
        sym setInfo ClassInfoType(parents, newScope, cclazz)
      }
    }

    cclazz
  }

  /** Perform the following tree transformations:
   *
   *  1. Add trees for abstract types (@see devirtualize),
   *     worker traits (@see devirtualize)
   *     and factories (@see addFactory)
   *
   *  2. Replace a new VC().init(...) where VC is a virtual class with new$VC(...)
   *
   *  3. Replace references to VC.this and VC.super where VC is a virtual class
   *     with VC$trait.this and VC$trait.super
   *
   *  4. Transform type references to virtual classes to type references of corresponding
   *     abstract types.
   */
  class DeVirtualizeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    // all code is executed at phase ownPhase.next

    /** Add trees for abstract types, worker traits, and factories (@see addFactory)
     *  to template body `stats'
     */
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
//      println("QUX!")
      val stats1 = stats flatMap transformStat map transform
      val newDefs = new ListBuffer[Tree]
      if (currentOwner.isClass && containsVirtuals(currentOwner)) {
        for (m <- classesInNeedOfFactories(currentOwner))
          newDefs += factoryDef(m)
      }
      if (newDefs.isEmpty) stats1
      else stats1 ::: newDefs.toList
    }

    /** The factory definition for virtual class `clazz' (@see addFactory)
     *  For a virtual class
     *
     *  attrs mods class VC[Ts] <: Ps { decls }
     *  with overridden classes _VC[Us]'s
     *
     * we need the following factory:
     *
     *  attrs mods3 def new$VC[Ts](): VC[Ts] = {
     *    class VC$fix extends _VC$trait's[Ts] with VC$trait[Ts] {
     *      override-bridges
     *    }
     *    new VC$fix
     *  }
     *
     * where
     *
     *  mods3  are the modifiers inherited to factories
     *  override-bridges are definitions that link every symbol in a worker trait
     *                   that was overriding something to the overridden symbol
     *                   //todo: not sure what happens with abstract override?
     */
    def factoryDef(clazz: Symbol): Tree = {
      val factorySym = factory(clazz)
      val cclazzSym = concreteClassSym(clazz, factorySym)
      val overrideBridges =
        for (m <- workerTrait(clazz).info.decls.toList if m hasFlag notOVERRIDE)
        yield overrideBridge(m, cclazzSym)
      val cclazzDef = ClassDef(cclazzSym, Modifiers(0), List(List()), List(List()), overrideBridges)
      val factoryExpr = atPos(factorySym.pos) {
        Block(List(cclazzDef), New(TypeTree(cclazzSym.tpe), List(List())))
      }
      DefDef(factorySym, vparamss => factoryExpr)
    }

    /** Create an override bridge for method `meth' in concrete class `cclazz'.
     *  An override bridge has the form
     *
     *   override f(xs1)...(xsN) = super.f(xs)...(xsN)
     */
    def overrideBridge(meth: Symbol, cclazz: Symbol) = atPos(meth.pos) {
      val bridge = meth.cloneSymbol(cclazz)
        .resetFlag(notOVERRIDE | notFINAL)
      val superRef: Tree = Select(Super(cclazz, nme.EMPTY.toTypeName), meth)
      DefDef(bridge, vparamss => (superRef /: vparamss)((fn, vparams) =>
        Apply(fn, vparams map (param => Ident(param) setPos param.pos))))
    }

    /** Replace definitions of virtual classes by definitions of corresponding
     *  abstract type and worker traits.
     */
    protected def transformStat(tree: Tree): List[Tree] = {
//      println("QUZZ!")
      tree match {
      case ClassDef(mods, name, tparams, templ @ Template(parents, self, body)) if (tree.symbol.isVirtualClass) =>
//      println("QUXX!")
        val clazz = tree.symbol
//      println("QUXY!")
        val absTypeSym = abstractType(clazz)
//      println("QUXY2!")
        val workerTraitSym = workerTrait(clazz)
//      println("QUXY3!")
        val abstypeDef = TypeDef(abstractType(clazz))
//      println("QUXY4!")
        val workerTraitDef = ClassDef(
          workerTraitSym,
          Modifiers(0),
          List(List()),
          List(List()),
          body)
//      println("QUXY5!")
        new ChangeOwnerTraverser(clazz, workerTraitSym)(
          new ChangeOwnerTraverser(templ.symbol, workerTraitDef.impl.symbol)(workerTraitDef.impl))
//      println("QUXY6!")
        List(abstypeDef, workerTraitDef) map localTyper.typed
      case _ =>
        List(tree)
      }
    }

    override def transform(tree: Tree): Tree = {
//      println("FOOB!")
      tree match {
        // Replace references to VC.this and VC.super where VC is a virtual class
        // with VC$trait.this and VC$trait.super
        case This(_) | Super(_, _) if tree.symbol.isVirtualClass =>
//      println("BARB!")
          tree setSymbol workerTrait(tree.symbol)

        // Replace a new VC().init() where VC is a virtual class with new$VC
        case Select(New(tpt), name) if (tree.symbol.isConstructor && tree.symbol.owner.isVirtualClass) =>
          val clazz = tpt.tpe.typeSymbol
          val fn = gen.mkAttributedRef(factory(clazz))
          val targs = tpt.tpe.typeArgs
          atPos(tree.pos) {
            localTyper.typed {
              Apply(
                if (targs.isEmpty) fn else TypeApply(fn, targs map TypeTree),
                List())
            }
          }

        case _ =>
//      println("BAZ!")
          super.transform(tree)
      }
    } setType devirtualizeMap(tree.tpe)

    override def transformUnit(unit: CompilationUnit) = atPhase(ownPhase.next) {
      super.transformUnit(unit)
    }
  }
}



/*
    class A {
      class C[X, Y](x: X) <: { var y = x ; def f(z: Y): X }
      class D[Y](z) extends C[Int, Y](f(z)) { override def f(z:Int) = 3 }
    }
    class B extends A {
      class C[X, Y](x: X) <: { def g = 2 }
    }

maps to:

  class A {
    type C[X, Y] <: CT[X, Y]

    trait CT[X, Y] { self: C => protected[this] val x: Int; val y = x; def f(z:Int) = z + 1 }

    type D <: C with DT

    trait DT extends { self: D => def f(z:Int) = z + 2 }

    trait preDT extends { self: D => val z: Int; val x = f(z) }

    def newC(x: Int): C
    def newD(x: Int): D

    //type C = CT
    //type D = C with DT

    class CC(_x:Int) extends { val x = _x } with CT

    def newC[X, Y](x:Int): C =
      new CC(x).asInstanceOf[C]

    class DC(_z:Int) extends { val z = _z } with preDT with CT with DT {
      override def f(z:Int) = super.f(z)
    }

    def newD(z:Int):D = new DC(z).asInstanceOf[D]
  }

  class B extends A {
    type C <: CT with CT2

    trait CT2 { self : C => def g = 2 }

    //type C = CT with CT2
    //type D = C with DT

    class CC2(_x:Int) extends { val x = _x } with CT with CT2

    def newC(x:Int): C = new CC2(x).asInstanceOf[C]

    class DC2(_z:Int) extends { val z = _z } with preDT with CT with CT2
      with DT { override def f(z:Int) = super.f(z) }

    def newD(z:Int): D = new DC2(z).asInstanceOf[D]
  }

*/
