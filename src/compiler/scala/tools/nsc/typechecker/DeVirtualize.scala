/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package typechecker

import symtab.Flags._
import transform.{InfoTransform, TypingTransformers}
import scala.tools.nsc.util.{Position, NoPosition}
import scala.collection.mutable.ListBuffer

abstract class DeVirtualize extends InfoTransform with TypingTransformers {

  import global._
  import definitions._
  import typer.{typed, typedOperator, atOwner}

  /** the following two members override abstract members in Transform */
  val phaseName: String = "devirtualize"

  /** The phase might set the following new flags: */
  override def phaseNextFlags: Long = notDEFERRED | notOVERRIDE | notFINAL | lateABSTRACT

  def newTransformer(unit: CompilationUnit): DeVirtualizeTransformer =
    new DeVirtualizeTransformer(unit)

  /** The class does not change base-classes of existing classes */
  override def changesBaseClasses = false

  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isThisSym && sym.owner.isVirtualClass) {
      val clazz = sym.owner
      intersectionType(
        List(
          appliedType(abstractType(clazz).typeConstructor, clazz.typeParams map (_.tpe)),
          clazz.tpe))
    } else devirtualizeMap(tp)

  /* todo:
     handle constructor arguments
     check: overriding classes must have same type params
     virtual classes cannot have self types
   */

  /** Do the following transformations everywhere in a type:
   *
   * 1. Replace a virtual class
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
   * The class symbol VC becomes the symbol of the workertrait.
   *
   *  dvm    is the devirtalization mapping which converts refs to
   *         virtual classes to refs to their abstract types (@see devirtualize)
   *  mods1  are the modifiers inherited to abstract types
   *  mods2  are the modifiers inherited to worker traits
   *  decls1 is decls but members that have an override modifier
   *         lose it and any final modifier as well.
   *
   * 2. For all virtual member classes VC which
   *    are not abstract and which are or inherit from a virtual class defined in current class
   *    add a factory (@see mkFactory)
   * 3. Convert TypeRef's to VC where VC is a virtual class to TypeRef's to AT, where AT
   *    is the abstract type corresponding to VC.
   *
   *  Note: If a class inherits vc's from two different paths, a vc in the
   *  inheriting class has to be created beforehand. This is done in phase ??? (NOT YET DONE!)
   *
   *  Note: subclasses of virtual classes are treated as if they are virtual.
   *  isVirtualClass returns true for them also.
   */
  object devirtualizeMap extends TypeMap {
    def apply(tp: Type): Type = mapOver(tp) match {
      case tp1 @ ClassInfoType(parents, decls0, clazz) =>
        var decls = decls0
        def enter(sym: Symbol) = // at next phase because names of worker traits change
          atPhase(ownPhase.next) { decls.enter(sym) }
        if (containsVirtuals(clazz)) {
          decls = new Scope
          for (m <- decls0.toList) {
            if (m.isVirtualClass) {
              m.setFlag(notDEFERRED | notFINAL | lateABSTRACT)
              enter(mkAbstractType(m))
            }
            enter(m)
          }
          for (m <- classesInNeedOfFactories(clazz))
            enter(mkFactory(m, clazz))
        }
        if (clazz.isVirtualClass) {
          println("virtual class: "+clazz+clazz.locationString)
          transformOwnerInfo(clazz)
          decls = new Scope

          // add virtual fields for all primary constructor parameters
          for (row <- paramTypesAndIndices(clazz.primaryConstructor.tpe, 0))
            for ((pt, i) <- row)
              enter(mkParamField(clazz, i, devirtualizeMap(pt)))

          // remove OVERRIDE from all workertrait members, except if they override a member in Object
          for (m <- decls0.toList) {
            if (!m.isConstructor) {
              if ((m hasFlag OVERRIDE) && m.overriddenSymbol(ObjectClass) == NoSymbol)
                m setFlag (notOVERRIDE | notFINAL)
              enter(m)
            }
          }
          if (clazz.thisSym == clazz) clazz.typeOfThis = clazz.thisType
            // ... to give a hook on which we can hang selftype transformers
          ClassInfoType(List(ObjectClass.tpe, ScalaObjectClass.tpe), decls, clazz)
        } else {
          ClassInfoType(parents map this, decls, clazz)
        }
      case tp1 @ TypeRef(pre, clazz, args) if clazz.isVirtualClass =>
        TypeRef(pre, abstractType(clazz), args)
      case tp1 =>
        tp1
    }
  }

  /** Transform owner of given clazz symbol */
  protected def transformOwnerInfo(clazz: Symbol) { atPhase(ownPhase.next) { clazz.owner.info } }

  /** Names of derived classes and factories */
  protected def concreteClassName(clazz: Symbol) =
    atPhase(ownPhase) { newTypeName(clazz.name+"$fix") }
  protected def factoryName(clazz: Symbol) =
    atPhase(ownPhase) { newTermName("new$"+clazz.name) }

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
    def isOverriddenVirtual(c: Symbol) =
      c.isVirtualClass && clazz.info.decl(c.name).isVirtualClass
    val xs = clazz.info.members.toList filter (x => x.isVirtualClass && !x.hasFlag(ABSTRACT))
    for (m <- clazz.info.members.toList;
         if (m.isVirtualClass && !(m hasFlag ABSTRACT) &&
             (m.info.baseClasses exists isOverriddenVirtual))) yield m
  }

  /** The abstract type corresponding to a virtual class. */
  protected def abstractType(clazz: Symbol): Symbol =  atPhase(ownPhase.next) {
    val abstpe = clazz.owner.info.decl(atPhase(ownPhase) { clazz.name })
    assert(abstpe.isAbstractType)
    abstpe
  }

  /** The factory corresponding to a virtual class. */
  protected def factory(clazz: Symbol, owner: Symbol) = atPhase(ownPhase.next) {
    val fsym = owner.info.member(factoryName(clazz))
    assert(fsym.isMethod, clazz)
    fsym
  }

  /** The name of the field representing a constructor parameter of a virtual class */
  protected def paramFieldName(clazz: Symbol, index: Int) = atPhase(ownPhase) {
    clazz.expandedName(newTermName("param$"+index))
  }

  /** The name of the field representing a constructor parameter of a virtual class */
  protected def fixParamName(index: Int) = newTermName("fix$"+index)

  /** The field representing a constructor parameter of a virtual class */
  protected def paramField(clazz: Symbol, index: Int) = atPhase(ownPhase.next) {
    clazz.info.decl(paramFieldName(clazz, index))
  }

  /** The flags that an abstract type can inherit from its virtual class */
  protected val absTypeFlagMask = AccessFlags | DEFERRED

  /** The flags that a factory method can inherit from its virtual class */
  protected val factoryFlagMask = AccessFlags

  /** Create a polytype with given type parameters and given type, or return just the type
   *  if type params is empty. */
  protected def mkPolyType(tparams: List[Symbol], tp: Type) =
    if (tparams.isEmpty) tp else PolyType(tparams, tp)

  /** A lazy type to complete `sym', which is is generated for virtual class
   *  `clazz'.
   *  The info of the symbol is computed by method `getInfo'.
   *  It is wrapped in copies of the type parameters of `clazz'.
   */
  abstract class PolyTypeCompleter(sym: Symbol, clazz: Symbol) extends LazyType {
    def getInfo: Type
    override val typeParams = cloneSymbols(clazz.typeParams, sym)
    override def complete(sym: Symbol) {
      sym.setInfo(
        mkPolyType(typeParams, getInfo.substSym(clazz.typeParams, typeParams)))
    }
  }

  protected def wasVirtualClass(sym: Symbol) = {
    sym.isVirtualClass || {
      sym.info
      sym hasFlag notDEFERRED
    }
  }

  protected def addOverriddenVirtuals(clazz: Symbol) = {
    (clazz.allOverriddenSymbols filter wasVirtualClass) ::: List(clazz)
  }

  protected def addOverriddenVirtuals(tpe: Type) = tpe match {
    case TypeRef(pre, sym, args) =>
      { for (vc <- sym.allOverriddenSymbols if wasVirtualClass(vc))
        yield typeRef(pre, vc, args) }.reverse ::: List(tpe)
  }

  protected def mkParamField(clazz: Symbol, index: Int, tpe: Type): Symbol = {
    val param = clazz.newMethod(clazz.pos, paramFieldName(clazz, index))
      .setFlag(PROTECTED | LOCAL | DEFERRED | EXPANDEDNAME | SYNTHETIC | STABLE)
    atPhase(ownPhase.next) {
      param.setInfo(PolyType(List(), tpe))
    }
    param
  }

  protected def mkAbstractType(clazz: Symbol): Symbol = {
    val cabstype = clazz.owner.newAbstractType(clazz.pos, clazz.name)
      .setFlag(clazz.flags & absTypeFlagMask | SYNTHETIC)
      .setAnnotations(clazz.annotations)
    atPhase(ownPhase.next) {
      cabstype setInfo new PolyTypeCompleter(cabstype, clazz) {
        def getInfo = {
          val parents1 = clazz.info.parents map {
            p => devirtualizeMap(p.substSym(clazz.typeParams, typeParams))
          }
          val parents2 = addOverriddenVirtuals(clazz) map {
            c => typeRef(clazz.owner.thisType, c, typeParams map (_.tpe))
          }
          TypeBounds(NothingClass.tpe, intersectionType(parents1 ::: parents2))
        }
      }
    }
  }

  protected def paramTypesAndIndices(tpe: Type, start: Int): List[List[(Type, Int)]] = tpe match {
    case PolyType(_, restpe) => paramTypesAndIndices(restpe, start)
    case MethodType(params, restpe) =>
      val end = start + params.length
      (tpe.paramTypes zip List.range(start, end)) :: paramTypesAndIndices(restpe, end)
    case _ =>
      List()
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
   *  v2w maps every virtual class to its workertrait and leaves other types alone.
   *
   *  @param  clazz    The virtual class for which factory is added
   *  @param  owner    The owner for which factory is added as a member
   *  @param  scope    The scope into which factory is entered
   */
  def mkFactory(clazz: Symbol, owner: Symbol): Symbol = {
    val pos = if (clazz.owner == owner) clazz.pos else owner.pos
    val factory = owner.newMethod(pos, factoryName(clazz))
      .setFlag(clazz.flags & factoryFlagMask | SYNTHETIC)
      .setAnnotations(clazz.annotations)
    factory setInfo new PolyTypeCompleter(factory, clazz) {
      private def copyType(tpe: Type): Type = tpe match {
        case MethodType(formals, restpe) => MethodType(formals, copyType(restpe))
        case PolyType(List(), restpe) => PolyType(List(), copyType(restpe))
        case PolyType(_, _) => throw new Error("bad case: "+tpe)
        case _ => owner.thisType.memberType(abstractType(clazz))
      }
      def getInfo = copyType(clazz.primaryConstructor.tpe)
    }
    factory
  }

  def removeDuplicates(ts: List[Type]): List[Type] = ts match {
    case List() => List()
    case t :: ts1 => t :: removeDuplicates(ts1 filter (_.typeSymbol != t.typeSymbol))
  }

  /** The concrete class symbol VC$fix in the factory symbol (@see mkFactory)
   *  @param clazz    the virtual class
   *  @param factory  the factory which returns an instance of this class
   */
  protected def mkConcreteClass(clazz: Symbol, factory: Symbol) = {
    val cclazz = factory.newClass(clazz.pos, concreteClassName(clazz))
      .setFlag(FINAL | SYNTHETIC)
      .setAnnotations(clazz.annotations)

    cclazz setInfo new LazyType {
      override def complete(sym: Symbol) {
        val parents1 = atPhase(ownPhase) {
          var superclazz = clazz
          do {
            superclazz = superclazz.info.parents.head.typeSymbol
          } while (wasVirtualClass(superclazz))
          val bcs = superclazz :: (clazz.info.baseClasses takeWhile (superclazz != )).reverse
          println("MKConcrete1 "+cclazz+factory.locationString+" "+bcs+" from "+clazz+clazz.locationString)
          println("MKConcrete2 "+cclazz+factory.locationString+" "+(bcs map factory.owner.thisType.memberType))
          bcs map factory.owner.thisType.memberType
        }
        atPhase(ownPhase.next) {
          val parents2 =
            removeDuplicates(parents1.flatMap(addOverriddenVirtuals))
            .map(_.substSym(clazz.typeParams, factory.typeParams))
          sym setInfo ClassInfoType(parents2, new Scope, cclazz)
        }
      }
    }

    cclazz
  }

  /** Perform the following tree transformations:
   *
   *  1. Add trees for abstract types (@see devirtualize),
   *     worker traits (@see devirtualize)
   *     and factories (@see mkFactory)
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

    /** Add trees for abstract types, worker traits, and factories (@see mkFactory)
     *  to template body `stats'
     */
    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      val stats1 = stats flatMap transformStat
      val fclasses = atPhase(ownPhase) {
        if (currentOwner.isClass && containsVirtuals(currentOwner)) classesInNeedOfFactories(currentOwner)
        else List()
      }
      val newDefs = fclasses map factoryDef
      if (newDefs.isEmpty) stats1 else stats1 ::: newDefs
    }

    def fixClassDef(clazz: Symbol, factory: Symbol): Tree = {
      val cclazz = mkConcreteClass(clazz, factory)
      val overrideBridges =
        for (m <- clazz.info.decls.toList if m hasFlag notOVERRIDE)
        yield overrideBridge(m, cclazz)

      val vparamss: List[List[ValDef]] = atPhase(ownPhase) {
        paramTypesAndIndices(clazz.primaryConstructor.tpe, 0) map {
          _ map {
          case (pt, i) =>
            atPos(factory.pos) {
              ValDef(Modifiers(PARAMACCESSOR | PRIVATE | LOCAL), fixParamName(i),
                     TypeTree(devirtualizeMap(pt)), EmptyTree)
            }
          }
        }
      }
      val pfields: List[DefDef] = atPhase(ownPhase) {
        paramTypesAndIndices(clazz.primaryConstructor.tpe, 0) flatMap {
          _ map {
          case (pt, i) =>
            val pfield = cclazz.newMethod(cclazz.pos, paramFieldName(clazz, i))
              .setFlag(PROTECTED | LOCAL | EXPANDEDNAME | SYNTHETIC | STABLE)
              .setInfo(PolyType(List(), pt))
            cclazz.info.decls enter pfield
            atPos(factory.pos) {
              DefDef(pfield, Ident(fixParamName(i)))
            }
          }
        }
      }
      atPos(clazz.pos) {
        ClassDef(cclazz, Modifiers(0), vparamss, List(List()), pfields ::: overrideBridges, clazz.pos.focus)
      }
    }


    /** The factory definition for virtual class `clazz' (@see mkFactory)
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
     *    new VC$fix.asInstanceOf[VC[Ts]]
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
      val factorySym = factory(clazz, currentOwner)
      val cclazzDef = fixClassDef(clazz, factorySym)
      println("Concrete: "+cclazzDef)
      val abstpeSym = abstractType(clazz)
      localTyper.typed {
        atPos(factorySym.pos) {
          DefDef(factorySym,
            Block(
              List(cclazzDef),
              TypeApply(
                Select(
                  gen.mkForwarder(
                    Select(New(TypeTree(cclazzDef.symbol.tpe)), nme.CONSTRUCTOR),
                    factorySym.paramss),
                  Any_asInstanceOf),
             List(
               TypeTree(
                 currentOwner.thisType.memberType(abstpeSym)
                 .substSym(abstpeSym.typeParams, factorySym.typeParams))))))
        }
      }
    }

    /** Create an override bridge for method `meth' in concrete class `cclazz'.
     *  An override bridge has the form
     *
     *   override f(xs1)...(xsN) = super.f(xs)...(xsN)
     */
    def overrideBridge(meth: Symbol, cclazz: Symbol) = atPos(meth.pos) {
      val bridge = meth.cloneSymbol(cclazz)
        .resetFlag(notOVERRIDE | notFINAL)
      cclazz.info.decls.enter(bridge)
      val superRef: Tree = Select(Super(cclazz, nme.EMPTY.toTypeName), meth)
      DefDef(bridge, gen.mkForwarder(superRef, bridge.paramss))
    }

    /** Replace definitions of virtual classes by definitions of corresponding
     *  abstract type and worker traits.
     *  Eliminate constructors of former virtual classes because these are now traits.
     */
    protected def transformStat(tree: Tree): List[Tree] = {
      val sym = tree.symbol
      tree match {
        case ClassDef(mods, name, tparams, templ)
        if (wasVirtualClass(sym)) =>
          val clazz = sym
          val absTypeSym = abstractType(clazz)
          val abstypeDef = TypeDef(absTypeSym)
          List(localTyper.typed(abstypeDef), transform(tree))
        case DefDef(_, nme.CONSTRUCTOR, _, _, _, _)
        if (wasVirtualClass(sym.owner)) =>
          if (atPhase(ownPhase)(sym != sym.owner.primaryConstructor))
            unit.error(tree.pos, "virtual classes cannot have auxiliary constructors")
          List()
        case _ =>
          List(transform(tree))
      }
    }

    override def transform(tree0: Tree): Tree = {
      val tree = super.transform(tree0)
      val sym = tree.symbol
      tree match {
        // Replace a new VC().init() where VC is a virtual class with new$VC
        case Apply(Select(New(tpt), name), args) if (sym.isConstructor && wasVirtualClass(sym.owner)) =>
          val clazz = sym.owner
          val fn =
            Select(
              gen.mkAttributedQualifier(tpt.tpe.prefix),
              factory(clazz, clazz.owner).name)
          println("fac "+factory(clazz, clazz.owner).tpe)
          val targs = tpt.tpe.typeArgs
          atPos(tree.pos) {
            localTyper.typed {
              val res =
                Apply(if (targs.isEmpty) fn else TypeApply(fn, targs map TypeTree), args)
                println("typing "+res+" from "+args)
                res
              }
            }

        case Template(parents, self, body) if (wasVirtualClass(sym.owner)) =>
          // add param field accessors
          val paramFieldAccessors = new ListBuffer[Tree]
          val paramFields = new ListBuffer[Tree]
          val presupers = new ListBuffer[Tree]
          val others = new ListBuffer[Tree]
          var paramFieldCount = 0
          for (stat <- body) {
            if (stat.symbol != null && (stat.symbol hasFlag PARAMACCESSOR))
              stat match {
                case pacc @ ValDef(mods, name, tpt, rhs) =>
                  pacc.symbol resetFlag PARAMACCESSOR setFlag PRESUPER
                  val pfield = paramField(sym.owner, paramFieldCount)
                  paramFieldCount += 1
                  pfield setPos pacc.pos
                  paramFields += localTyper.typed(DefDef(pfield, EmptyTree))
                  val pfieldRef = localTyper.typed {
                    atPos(pacc.pos) {
                      Select(This(sym.owner), pfield)
                    }
                  }
                  paramFieldAccessors += treeCopy.ValDef(pacc, mods, name, tpt, pfieldRef)
                case _ =>
                  stat.symbol resetFlag PARAMACCESSOR // ??? can we do this
                  others += stat
              }
            else
              (if (stat.symbol != null && (stat.symbol hasFlag PRESUPER)) presupers else others) += stat
          }
          treeCopy.Template(tree, parents, self,
                            paramFieldAccessors.toList :::
                            presupers.toList :::
                            paramFields.toList :::
                            others.toList)
        case _ =>
          tree setType atPhase(ownPhase)(devirtualizeMap(tree.tpe))
      }
    }
    override def transformUnit(unit: CompilationUnit) = atPhase(ownPhase.next) {
      super.transformUnit(unit)
    }
  }
}



/*
    class A {
      trait C[X, Y] <: {
        var x: X
        def f(y: Y): X = { println("A.T"); x }
      }
      class D[X](xp: X) extends C[X, Int] {
        var x: X = xp
        override def f(y: Int) = { println(y); super.f(y) }
      }
    }
    class B extends A {
      override trait C[X, Y] <: {
        override def f(y: Y): X = { println("B.T"); super.f(y) }
        def g: X = x
      }
    }
    object Test extends B {
      val c = new D[String]("OK")
      println(c.g)
      println(c.f(42))
    }

maps to:

  class A {
    type C[X, Y] <: C$trait[X, Y]

    trait C$trait[X, Y] { this: C with C$trait =>
      var x: X
      def f(y: Y): X = { println("A.T"); x }
    }

    class D[X](xp: X) extends C[X, Int] {
      var x: X = xp
      override def f(y: Int) = { println(y); super.f(y) }
    }

      protected[this] val x: Int; val y = x; def f(z:Int) = z + 1 }

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
