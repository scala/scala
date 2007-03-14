/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.transform

import scala.collection.mutable.{HashMap,ListBuffer}
import scala.tools.nsc.util.Position
import symtab._
import Flags._

abstract class Erasure extends AddInterfaces with typechecker.Analyzer {
  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.{typed}             // methods to type trees
  import posAssigner.atPos         // for filling in tree positions

  val phaseName: String = "erasure"

  def newTransformer(unit: CompilationUnit): Transformer =
    new ErasureTransformer(unit)

// -------- erasure on types --------------------------------------------------------

  /** <p>
   *    The erasure <code>|T|</code> of a type <code>T</code>. This is:
   *  </p>
   *  <ul>
   *    <li>For a constant type, itself.</li>
   *    <li>For a type-bounds structure, the erasure of its upper bound.</li>
   *    <li>For every other singleton type, the erasure of its supertype.</li>
   *    <li>
   *      For a typeref <code>scala.Array[T]</code> where <code>T</code> is
   *      an abstract type, <code>scala.runtime.BoxedArray</code>.
   *    </li>
   *    <li>
   *   - For a typeref scala.Array[T] where T is not an abstract type, scala.Array[|T|].
   *   - For a typeref scala.Any or scala.AnyVal, java.lang.Object.
   *   - For a typeref scala.Unit, scala.runtime.BoxedUnit.
   *   - For a typeref P.C[Ts] where C refers to a class, |P|.C.
   *   - For a typeref P.C[Ts] where C refers to an alias type, the erasure of C's alias.
   *   - For a typeref P.C[Ts] where C refers to an abstract type, the
   *     erasure of C's upper bound.
   *   - For a non-empty type intersection (possibly with refinement),
   *     the erasure of its first parent.
   *   - For an empty type intersection, java.lang.Object
   *   - For a method type (Fs)scala.Unit, (|Fs|)scala#Unit.
   *   - For any other method type (Fs)Y, (|Fs|)|T|.
   *   - For a polymorphic type, the erasure of its result type
   *   - For the class info type of java.lang.Object, the same type without any parents
   *   - For a class info type of a value class, the same type without any parents
   *   - For any other class info type with parents Ps, the same type with
   *     parents |Ps|, but with duplicate references of Object removed.
   *   - for all other types, the type itself (with any sub-components erased)
   *    </li>
   *  </ul>
   */
  val erasure = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case ConstantType(_) =>
        tp
      case st: SubType =>
        apply(st.supertype)
      case TypeRef(pre, sym, args) =>
        if (sym == ArrayClass)
          args.head match {
            case TypeRef(_, tvar, _) if (tvar.isAbstractType) => erasedTypeRef(BoxedArrayClass)
            case _ => typeRef(apply(pre), sym, args map this)
          }
        else if (sym == AnyClass || sym == AnyValClass) erasedTypeRef(ObjectClass)
        else if (sym == UnitClass) erasedTypeRef(BoxedUnitClass)
        else if (sym.isClass)
          typeRef(apply(if (sym.owner.isClass) sym.owner.tpe else pre), sym, List())
        else apply(sym.info)
      case PolyType(tparams, restpe) =>
        apply(restpe)
      case MethodType(formals, restpe) =>
        MethodType(
          formals map apply,
          if (restpe.symbol == UnitClass) erasedTypeRef(UnitClass) else apply(restpe))
      case RefinedType(parents, decls) =>
        if (parents.isEmpty) erasedTypeRef(ObjectClass)
        else apply(parents.head)
      case ClassInfoType(parents, decls, clazz) =>
        ClassInfoType(
          if ((clazz == ObjectClass) || (isValueType(clazz))) List()
          else if (clazz == ArrayClass) List(erasedTypeRef(ObjectClass))
          else removeDoubleObject(parents map this),
          decls, clazz)
      case _ =>
        mapOver(tp)
    }
  }

  /** Type reference after erasure */
  def erasedTypeRef(sym: Symbol): Type =
    typeRef(erasure(sym.owner.tpe), sym, List())

  /** Remove duplicate references to class Object in a list of parent classes
   * todo: needed?
   */
  private def removeDoubleObject(tps: List[Type]): List[Type] = tps match {
    case List() => List()
    case tp :: tps1 =>
      if (tp.symbol == ObjectClass) tp :: tps1.filter(.symbol.!=(ObjectClass))
      else tp :: removeDoubleObject(tps1)
  }

  /** <p>
   *    The symbol's erased info. This is the type's erasure, except for the
   *    following symbols:
   *  </p>
   *  <ul>
   *    <li>
   *      For <code>$asInstanceOf</code> : <code>[T]T</code>
   *    </li>
   *    <li>
   *      For <code>$isInstanceOf</code> : <code>[T]scala#Boolean</code>
   *    </li>
   *    <li>
   *      For class <code>Array</code> : <code>[T]C</code> where
   *      <code>C</code> is the erased classinfo of the <code>Array</code> class
   *    </li>
   *    <li>
   *      For <code>Array[T].&lt;init&gt;</code> : <code>{scala#Int)Array[T]</code>
   *    </li>
   *    <li>
   *      For a type parameter : A type bounds type consisting of the erasures
   *      of its bounds.
   *    </li>
   *  </ul>
   */
  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym == Object_asInstanceOf)
      sym.info
    else if (sym == Object_isInstanceOf || sym == ArrayClass)
      PolyType(sym.info.typeParams, erasure(sym.info.resultType))
    else if (sym.isAbstractType)
      mkTypeBounds(WildcardType, WildcardType)
    else if (sym.isTerm && sym.owner == ArrayClass) {
      if (sym.isClassConstructor)
        tp match {
          case MethodType(formals, TypeRef(pre, sym, args)) =>
            MethodType(formals map erasure, typeRef(erasure(pre), sym, args))
        }
      else if (sym.name == nme.apply)
        tp
      else if (sym.name == nme.update)
        tp match {
          case MethodType(List(index, tvar), restpe) =>
            MethodType(List(erasure(index), tvar), erasedTypeRef(UnitClass))
        }
      else erasure(tp)
    } else
      transformMixinInfo(erasure(tp));

  val deconstMap = new TypeMap {
   def apply(tp: Type): Type = tp match {
     case PolyType(_, _) => mapOver(tp)
     case MethodType(_, _) => mapOver(tp)
     case _ => tp.deconst
   }
  }

  private def isSeqClass(sym: Symbol) =
    (SeqClass isNonBottomSubClass sym) && (sym != ObjectClass)

// -------- boxing/unboxing --------------------------------------------------------

  override def newTyper(context: Context) = new Eraser(context)

  /** The modifier typer which retypes with erased types. */
  class Eraser(context: Context) extends Typer(context) {

    /** Box `tree' of unboxed type */
    private def box(tree: Tree): Tree =
      typed {
        atPos(tree.pos) {
          val sym = tree.tpe.symbol;
          if (sym == UnitClass) {
            if (treeInfo.isPureExpr(tree)) gen.mkAttributedRef(BoxedUnit_UNIT)
            else Block(List(tree), gen.mkAttributedRef(BoxedUnit_UNIT))
          } else if (sym == ArrayClass) {
            val elemClass = tree.tpe.typeArgs.head.symbol;
            val boxedClass = if (isValueClass(elemClass)) boxedArrayClass(elemClass)
                             else BoxedObjectArrayClass;
            Apply(Select(New(TypeTree(boxedClass.tpe)), nme.CONSTRUCTOR), List(tree))
          } else {
            Apply(gen.mkAttributedRef(boxMethod(tree.tpe.symbol)), List(tree)).
              setPos(tree.pos) setType ObjectClass.tpe
          }
        }
      }

    /** generate  ScalaRuntime.boxArray(tree) */
    private def boxArray(tree: Tree): Tree =
      typed {
        atPos(tree.pos) {
          gen.mkRuntimeCall(nme.boxArray, List(tree))
        }
      }

    /** Unbox <code>tree</code> of boxed type to expected type <code>pt</code>.
     *
     *  @param tree the given tree
     *  @param pt   the expected type.
     *  @return     the unboxed tree
     */
    private def unbox(tree: Tree, pt: Type): Tree =
      typed {
        atPos(tree.pos) {
          if (pt.symbol == UnitClass) {
            if (treeInfo.isPureExpr(tree)) Literal(())
            else Block(List(tree), Literal(()))
          }
          else if (pt.symbol == ArrayClass) {
            val tree1 = adaptToType(tree, BoxedArrayClass.tpe)
            gen.mkRuntimeCall(nme.arrayValue, List(tree1, Literal(pt.typeArgs.head)))
          }
          else {
            atPos(tree.pos) {
              Apply(gen.mkAttributedRef(unboxMethod(pt.symbol)), List(tree)) setType pt
            }
          }
        }
      }

    /** <p>
     *    Generate a cast operation from <code>tree.tpe</code> to <code>pt</code>.
     *    The following cases need to be treated specially:
     *  </p>
     *  <table>
     *    <tr>
     *      <td><code>Object -> Array</code></td>
     *      <td>(might be a boxedarray)</td>
     *    </tr>
     *    <tr>
     *      <td><code>Object -> Boxed*Array</code></td>
     *      <td>(might be an array, which nees to be boxed)</td>
     *    </tr>
     *    <tr>
     *      <td><code>Object -> Seq, Iterable</code></td>
     *      <td>(might be an array, which needs to be boxed)</td>
     *    </tr>
     *  </table>
     */
    private def cast(tree: Tree, pt: Type): Tree =
      if (tree.tpe.symbol == ObjectClass) {
        if (pt.symbol == ArrayClass)
          typed {
            atPos(tree.pos) {
              gen.evalOnce(tree, context.owner, context.unit) { x =>
                gen.mkAttributedCast(
                  If(
                    Apply(
                      TypeApply(
                        Select(x(), Object_isInstanceOf),
                        List(TypeTree(BoxedArrayClass.tpe))),
                      List()),
                    unbox(gen.mkAttributedCast(x(), BoxedArrayClass.tpe), pt),
                    x()),
                  pt)
              }
            }
          }
        else if (pt.symbol isNonBottomSubClass BoxedArrayClass)
          typed {
            atPos(tree.pos) {
              gen.evalOnce(tree, context.owner, context.unit) { x =>
                gen.mkAttributedCast(
                  If(
                    Apply(
                      TypeApply(
                        Select(x(), Object_isInstanceOf),
                        List(TypeTree(BoxedArrayClass.tpe))),
                      List()),
                    x(),
                    boxArray(x())),
                  pt)
              }
            }
          }
        else if (isSeqClass(pt.symbol))
          typed {
            atPos(tree.pos) {
              gen.evalOnce(tree, context.owner, context.unit) { x =>
                gen.mkAttributedCast(
                  If(
                    Apply(
                      TypeApply(
                        Select(x(), Object_isInstanceOf),
                        List(TypeTree(pt))),
                      List()),
                    x(),
                    boxArray(x())),
                  pt)
              }
            }
          }
        else gen.mkAttributedCast(tree, pt)
      } else gen.mkAttributedCast(tree, pt)

    /** Is symbol a member of unboxed arrays (which will be expanded directly
     *  later)?
     *
     *  @param sym ..
     *  @return    <code>true</code> if ..
     */
    private def isUnboxedArrayMember(sym: Symbol) =
      sym.name == nme.apply || sym.name == nme.length || sym.name == nme.update ||
      sym.owner == ObjectClass

    private def isUnboxedValueMember(sym: Symbol) =
      sym != NoSymbol && isValueClass(sym.owner)

    /** Adapt <code>tree</code> to expected type <code>pt</code>.
     *
     *  @param tree the given tree
     *  @param pt   the expected type.
     *  @return     the adapted tree
     */
    private def adaptToType(tree: Tree, pt: Type): Tree = {
      if (settings.debug.value && pt != WildcardType)
        log("adapting " + tree + ":" + tree.tpe + " : " +  tree.tpe.parents + " to " + pt)//debug
      if (tree.tpe <:< pt)
        tree
      else if (isUnboxedClass(tree.tpe.symbol) && !isUnboxedClass(pt.symbol))
        adaptToType(box(tree), pt)
      else if (tree.tpe.isInstanceOf[MethodType] && tree.tpe.paramTypes.isEmpty) {
        if (!tree.symbol.isStable) assert(false, "adapt "+tree+":"+tree.tpe+" to "+pt)
        adaptToType(Apply(tree, List()) setPos tree.pos setType tree.tpe.resultType, pt)
      } else if (pt <:< tree.tpe)
        cast(tree, pt)
      else if (isUnboxedClass(pt.symbol) && !isUnboxedClass(tree.tpe.symbol))
        adaptToType(unbox(tree, pt), pt)
      else
        cast(tree, pt)
    }

    /** <p>
     *    Replace member references as follows:
     *  </p>
     *  <ul>
     *    <li>
     *      <code>x == y</code> for <code>==</code> in class <code>Any</code>
     *      becomes <code>x equals y</code> with <code>equals</code> in class
     *      <code>Object</code>.
     *    </li>
     *    <li>
     *      <code>x != y</code> for <code>!=</code> in class <code>Any</code>
     *      becomes <code>!(x equals y)</code> with <code>equals</code> in
     *      class <code>Object</code>.
     *    </li>
     *    <li>
     *      <code>new BoxedArray.&lt;init&gt;(len)</code> becomes
     *      <code>new BoxedAnyArray.&lt;init&gt;(len): BoxedArray</code>
     *      (the widening typing is necessary so that subsequent member
     *      symbols stay the same)
     *    </li>
     *    <li>
     *      <code>x.asInstanceOf[T]</code> and <code>x.asInstanceOf$erased[T]</code>
     *      become <code>x.$asInstanceOf[T]</code>
     *    </li>
     *    <li>
     *      <code>x.isInstanceOf[T]</code> and <code>x.isInstanceOf$erased[T]</code>
     *      become <code>x.$isInstanceOf[T]</code>
     *    </li>
     *    <li>
     *      <code>x.m</code> where <code>m</code> is some other member of
     *      <code>Any</code> becomes <code>x.m</code> where m is a member
     *      of class <code>Object</code>
     *    </li>
     *    <li>
     *      <code>x.m</code> where <code>x</code> has unboxed value type
     *      <code>T</code> and <code>m</code> is not a directly translated
     *      member of <code>T</code> becomes <code>T.box(x).m</code>
     *    </li>
     *    <li>
     *      <code>x.m</code> where <code>x</code> has type <code>Array[T]</code>
     *      and <code>m</code> is not a directly translated member of
     *      <code>Array</code> becomes <code>new BoxedTArray.<init>(x).m</code>
     *    </li>
     *    <li>
     *      <code>x.m</code> where <code>x</code> is a reference type and
     *      <code>m</code> is a directly translated member of value type
     *      <code>T</code> becomes <code>x.TValue().m</code>
     *    </li>
     *    <li>
     *      All forms of <code>x.m</code> where <code>x</code> is a boxed type
     *      and <code>m</code> is a member of an unboxed class become
     *      <code>x.m</code> where <code>m</code> is the corresponding member
     *      of the boxed class.
     *    </li>
     *  </ul>
     */
    private def adaptMember(tree: Tree): Tree = {
      //Console.println("adaptMember: " + tree);
      tree match {
        case Apply(Select(New(tpt), name), args) if (tpt.tpe.symbol == BoxedArrayClass) =>
          assert(name == nme.CONSTRUCTOR);
          atPos(tree.pos) {
            Typed(Apply(Select(New(TypeTree(BoxedAnyArrayClass.tpe)), name), args), tpt)
          }
        case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List())
        if ((tree.symbol == Any_asInstanceOf || tree.symbol == Any_asInstanceOfErased)) =>
          val qual1 = typedQualifier(qual)
          val qualClass = qual1.tpe.symbol
          val targClass = targ.tpe.symbol
          if (isNumericValueClass(qualClass) && isNumericValueClass(targClass))
            // convert numeric type casts
            atPos(tree.pos)(Apply(Select(qual1, "to" + targClass.name), List()))
          else if (isValueType(targClass) ||
                   (targClass == ArrayClass && (qualClass isNonBottomSubClass BoxedArrayClass)))
            unbox(qual1, targ.tpe)
          else if (targClass == ArrayClass && qualClass == ObjectClass || isSeqClass(targClass))
            cast(qual1, targ.tpe)
          else
            tree
        case Select(qual, name) if (name != nme.CONSTRUCTOR) =>
          if (tree.symbol == NoSymbol)
            tree
          else if (tree.symbol == Any_asInstanceOf || tree.symbol == Any_asInstanceOfErased)
            adaptMember(atPos(tree.pos)(Select(qual, Object_asInstanceOf)))
          else if (tree.symbol == Any_isInstanceOf || tree.symbol == Any_isInstanceOfErased)
            adaptMember(atPos(tree.pos)(Select(qual, Object_isInstanceOf)))
          else if (tree.symbol.owner == AnyClass)
            adaptMember(atPos(tree.pos)(Select(qual, getMember(ObjectClass, name))))
          else {
            var qual1 = typedQualifier(qual);
            if ((isValueType(qual1.tpe.symbol) && !isUnboxedValueMember(tree.symbol)) ||
                (qual1.tpe.symbol == ArrayClass && !isUnboxedArrayMember(tree.symbol)))
              qual1 = box(qual1);
            else if (!isValueType(qual1.tpe.symbol) && isUnboxedValueMember(tree.symbol))
              qual1 = unbox(qual1, tree.symbol.owner.tpe)
            else if (tree.symbol.owner == ArrayClass && qual1.tpe.symbol == ObjectClass)
              qual1 = cast(qual1, BoxedArrayClass.tpe)

            if (isUnboxedClass(tree.symbol.owner) && !isUnboxedClass(qual1.tpe.symbol))
              tree.symbol = NoSymbol
            else if (qual1.tpe.isInstanceOf[MethodType] && qual1.tpe.paramTypes.isEmpty) {
              assert(qual1.symbol.isStable, qual1.symbol);
              qual1 = Apply(qual1, List()) setPos qual1.pos setType qual1.tpe.resultType;
            } else if (!(qual1.isInstanceOf[Super] || (qual1.tpe.symbol isSubClass tree.symbol.owner)))
              qual1 = cast(qual1, tree.symbol.owner.tpe);
            copy.Select(tree, qual1, name)
          }
        case _ =>
          tree
      }
    }

    /** A replacement for the standard typer's <code>adapt</code> method.
     *
     *  @param tree ...
     *  @param mode ...
     *  @param pt   ...
     *  @return     the adapted tree
     */
    override protected def adapt(tree: Tree, mode: int, pt: Type): Tree =
      adaptToType(tree, pt)

    /** A replacement for the standard typer's `typed1' method */
    override protected def typed1(tree: Tree, mode: int, pt: Type): Tree = {
      var tree1 = try {
        super.typed1(adaptMember(tree), mode, pt)
      } catch {
        case ex: Throwable =>
          //if (settings.debug.value)
            Console.println("exception when typing " + tree);
          throw ex
      }
      def adaptCase(cdef: CaseDef): CaseDef = {
        val body1 = adaptToType(cdef.body, tree1.tpe)
        copy.CaseDef(cdef, cdef.pat, cdef.guard, body1) setType body1.tpe
      }
      def adaptBranch(branch: Tree): Tree =
        if (branch == EmptyTree) branch else adaptToType(branch, tree1.tpe);
      tree1 match {
        case If(cond, thenp, elsep) =>
          copy.If(tree1, cond, adaptBranch(thenp), adaptBranch(elsep))
        case Match(selector, cases) =>
          copy.Match(tree1, selector, cases map adaptCase)
        case Try(block, catches, finalizer) =>
          copy.Try(tree1, adaptBranch(block), catches map adaptCase, finalizer)
        case Ident(_) | Select(_, _) =>
          if (tree1.symbol hasFlag OVERLOADED) {
            val first = tree1.symbol.alternatives.head
            val sym1 = tree1.symbol.filter {
              alt => alt == first || !(first.tpe looselyMatches alt.tpe)
            }
            if (tree.symbol ne sym1) {
              tree1.symbol = sym1
              tree1.tpe = sym1.tpe
            }
          }
          tree1
        case _ =>
          tree1
      }
    }
  }

  /** The erasure transformer */
  class ErasureTransformer(unit: CompilationUnit) extends Transformer {

    /** <p>
     *    Emit an error if there is a double definition. This can happen in
     *    the following circumstances:
     *  </p>
     *  <ul>
     *    <li>
     *      A template defines two members with the same name and erased type.
     *    </li>
     *    <li>
     *      A template defines and inherits two members <code>m</code> with
     *      different types, but their erased types are the same.
     *    </li>
     *    <li>
     *      A template inherits two members <code>m</code> with different
     *      types, but their erased types are the same.
     *    </li>
     *  </ul>
     */
    private def checkNoDoubleDefs(root: Symbol): unit = {
      def doubleDefError(sym1: Symbol, sym2: Symbol): unit = {
        val tpe1 = atPhase(currentRun.refchecksPhase.next)(root.thisType.memberType(sym1));
        val tpe2 = atPhase(currentRun.refchecksPhase.next)(root.thisType.memberType(sym2));
        unit.error(
          if (sym1.owner == root) sym1.pos else root.pos,
          (if (sym1.owner == sym2.owner) "double definition:\n"
           else if (sym1.owner == root) "name clash between defined and inherited member:\n"
           else "name clash between inherited members:\n") +
          sym1 + ":" + tpe1 +
            (if (sym1.owner == root) "" else sym1.locationString) + " and\n" +
          sym2 + ":" + tpe2 +
            (if (sym2.owner == root) " at line " + Position.line(unit.source, sym2.pos) else sym2.locationString) +
          "\nhave same type" +
          (if (tpe1 =:= tpe2) "" else " after erasure: " + atPhase(phase.next)(sym1.tpe)))
        sym1.setInfo(ErrorType)
      }

      val decls = root.info.decls
      var e = decls.elems
      while (e ne null) {
        if (e.sym.isTerm && !e.sym.isConstructor) {
          var e1 = decls.lookupNextEntry(e)
          while (e1 ne null) {
            if (atPhase(phase.next)(e1.sym.info =:= e.sym.info)) doubleDefError(e.sym, e1.sym)
            e1 = decls.lookupNextEntry(e1)
          }
        }
        e = e.next
      }

      val opc = new overridingPairs.Cursor(root) {
        override def exclude(sym: Symbol): boolean =
          !sym.isTerm || (sym hasFlag (PRIVATE | BRIDGE)) || super.exclude(sym)
        override def matches(sym1: Symbol, sym2: Symbol): boolean =
          atPhase(phase.next)(sym1.tpe =:= sym2.tpe)
      }
      while (opc.hasNext) {
        if (!atPhase(currentRun.refchecksPhase.next)(
              root.thisType.memberType(opc.overriding) matches
              root.thisType.memberType(opc.overridden))) {
          if (settings.debug.value)
            log("" + opc.overriding.locationString + " " +
                     opc.overriding.infosString +
                     opc.overridden.locationString + " " +
                     opc.overridden.infosString)
            doubleDefError(opc.overriding, opc.overridden)
        }
        opc.next
      }
    }

/*
      for (val bc <- root.info.baseClasses.tail; val other <- bc.info.decls.toList) {
        if (other.isTerm && !other.isConstructor && !(other hasFlag (PRIVATE | BRIDGE))) {
          for (val member <- root.info.nonPrivateMember(other.name).alternatives) {
            if (member != other &&
                !(member hasFlag BRIDGE) &&
                atPhase(phase.next)(member.tpe =:= other.tpe) &&
                !atPhase(refchecksPhase.next)(
                  root.thisType.memberType(member) matches root.thisType.memberType(other))) {
              if (settings.debug.value) log("" + member.locationString + " " + member.infosString + other.locationString + " " + other.infosString);
              doubleDefError(member, other)
            }
          }
        }
      }
*/

    /** <p>
     *    Add bridge definitions to a template. This means:
     *  </p>
     *  <p>
     *    If there is a concrete member <code>m</code> which overrides a
     *    member in a base class of the template, and the erased types of
     *    the two members differ, and the two members are not inherited or
     *    defined by some parent class of the template, then a bridge from
     *    the overridden member <code>m1</code> to the member <code>m0</code>
     *    is added. The bridge has the erased type of <code>m1</code> and
     *    forwards to <code>m0</code>.
     *  </p>
     *  <p>
     *    No bridge is added if there is already a bridge to <code>m0</code>
     *    with the erased type of <code>m1</code> in the template.
     *  </p>
     */
    private def bridgeDefs(owner: Symbol): List[Tree] = {
      //Console.println("computing bridges for " + owner)//DEBUG
      assert(phase == currentRun.erasurePhase)
      val site = owner.thisType
      val bridgesScope = newScope
      val bridgeTarget = new HashMap[Symbol, Symbol]
      var bridges: List[Tree] = List()
      val opc = atPhase(currentRun.explicitOuterPhase) {
        new overridingPairs.Cursor(owner) {
          override def parents: List[Type] = List(owner.info.parents.head)
          override def exclude(sym: Symbol): boolean =
            !sym.isMethod || (sym hasFlag (PRIVATE | BRIDGE)) || super.exclude(sym)
        }
      }
      while (opc.hasNext) {
        val member = opc.overriding
        val other = opc.overridden
        //Console.println("bridge? " + member + ":" + member.tpe + member.locationString + " to " + other + ":" + other.tpe + other.locationString);//DEBUG
        if (!atPhase(currentRun.explicitOuterPhase)(member hasFlag DEFERRED)) {
          val otpe = erasure(other.tpe);
          val bridgeNeeded = atPhase(phase.next) (
            !(other.tpe =:= member.tpe) &&
            !(deconstMap(other.tpe) =:= deconstMap(member.tpe)) &&
            { var e = bridgesScope.lookupEntry(member.name)
              while ((e ne null) && !((e.sym.tpe =:= otpe) && (bridgeTarget(e.sym) == member)))
                e = bridgesScope.lookupNextEntry(e);
              (e eq null)
            }
          );
          if (bridgeNeeded) {
            val bridge = other.cloneSymbolImpl(owner)
              .setPos(owner.pos)
              .setFlag(member.flags | BRIDGE)
              .resetFlag(ACCESSOR | DEFERRED | lateDEFERRED)
              .setInfo(otpe);
            bridgeTarget(bridge) = member
            atPhase(phase.next) { owner.info.decls.enter(bridge) }
            bridgesScope enter bridge
            bridges =
              atPhase(phase.next) {
                atPos(bridge.pos) {
                  val bridgeDef =
                    DefDef(bridge, vparamss =>
                      member.tpe match {
                        case MethodType(List(), ConstantType(c)) => Literal(c)
                        case _ =>
                          (((Select(This(owner), member): Tree) /: vparamss)
                             ((fun, vparams) => Apply(fun, vparams map Ident)))
                      });
                  if (settings.debug.value)
                    log("generating bridge from " + other + "(" + Flags.flagsToString(bridge.flags)  + ")" + ":" + otpe + other.locationString + " to " + member + ":" + erasure(member.tpe) + member.locationString + " =\n " + bridgeDef);
                  bridgeDef
                }
              } :: bridges;
          }
        }
        opc.next
      }
      bridges
    }
/*
      for (val bc <- site.baseClasses.tail; val other <- bc.info.decls.toList) {
        if (other.isMethod && !other.isConstructor) {
          for (val member <- site.nonPrivateMember(other.name).alternatives) {
            if (member != other &&
                !(member hasFlag DEFERRED) &&
                (site.memberType(member) matches site.memberType(other)) &&
                !(site.parents exists (p =>
                  (p.symbol isSubClass member.owner) && (p.symbol isSubClass other.owner)))) {
...
             }
          }
*/

    def addBridges(stats: List[Tree], base: Symbol): List[Tree] =
      if (base.isTrait) stats
      else {
        val bridges = bridgeDefs(base)
        if (bridges.isEmpty) stats else stats ::: bridges
      }

    /** <p>
     *    Transform tree at phase <code>erasure</code> before retyping it.
     *    This entails the following:
     *  </p>
     *  <ul>
     *    <li>Remove all type parameters in class and method definitions.</li>
     *    <li>Remove all abstract and alias type definitions.</li>
     *    <li>
     *      Remove all type applications other than those involving a type
     *      test or cast.
     *    </li>
     *    <li>
     *      Remove all empty trees in statements and definitions in a
     *      <code>PackageDef</code>.
     *    </li>
     *    <li>Check that there are no double definitions in a template.</li>
     *    <li>Add bridge definitions to a template.</li>
     *    <li>
     *      Replace all types in type nodes and the <code>EmptyTree</code>
     *      object by their erasure. Type nodes of type <code>Unit</code>
     *      representing result types of methods are left alone.
     *    </li>
     *    <li>
     *      Reset all other type attributes to <code>null</code>, thus
     *      enforcing a retyping.
     *    </li>
     *  </ul>
     */
    private val preTransformer = new Transformer {
      override def transform(tree: Tree): Tree = {
        if (tree.symbol == ArrayClass) return tree
        val tree1 = tree match {
          case ClassDef(mods, name, tparams, self, impl) =>
            if (settings.debug.value)
              log("defs of " + tree.symbol + " = " + tree.symbol.info.decls)
            copy.ClassDef(tree, mods, name, List(), emptyValDef, impl)
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            copy.DefDef(tree, mods, name, List(), vparamss, tpt, rhs)
          case AbsTypeDef(_, _, _, _) =>
            EmptyTree
          case AliasTypeDef(_, _, _, _) =>
            EmptyTree
          case TypeApply(fun, args) if (fun.symbol.owner != AnyClass &&
                                        fun.symbol != Object_asInstanceOf &&
                                        fun.symbol != Object_isInstanceOf) =>
            // leave all other type tests/type casts, remove all other type applications
            fun
          case Apply(fn, args) =>
            def isGenericArray(tpe: Type): boolean = erasure(tpe).symbol == BoxedArrayClass
            if (fn.hasSymbol &&
                fn.symbol.name == nme.arraycopy &&
                fn.symbol.owner.name == nme.System.toTypeName &&
                fn.symbol.owner.owner == JavaLangPackage.tpe.symbol &&
                args.length == 5 &&
                (isGenericArray(args(0).tpe) || isGenericArray(args(2).tpe)))
              unit.warning(tree.pos,
                           "System.arraycopy should be applied only to arrays with fixed element types;\n" +
                           "use Array.copy instead")
            if (fn.symbol == Any_asInstanceOf || fn.symbol == Any_asInstanceOfErased)
              fn match {
                case TypeApply(Select(qual, _), List(targ)) =>
                  assert(qual.tpe ne null, tree)
                  if (qual.tpe <:< targ.tpe) atPos(tree.pos) { Typed(qual, TypeTree(qual.tpe)) }
                  else tree
              }
              // todo: get rid of instanceOfErased
              // todo: also handle the case where the singleton type is buried in a compound
	    else if (fn.symbol == Any_isInstanceOf || fn.symbol == Any_isInstanceOfErased)
              fn match {
                case TypeApply(sel @ Select(qual, name), List(targ)) =>
                  def mkIsInstanceOf(q: () => Tree)(tp: Type): Tree =
                    Apply(
                      TypeApply(
                        Select(q(), Object_isInstanceOf) setPos sel.pos,
                        List(TypeTree(tp) setPos targ.pos)) setPos fn.pos,
                      List()) setPos tree.pos
                  targ.tpe match {
                    case SingleType(pre, sym) =>
                      val cmpOp = if (targ.tpe <:< AnyValClass.tpe) Any_equals else Object_eq
                      atPos(tree.pos) {
                        Apply(Select(qual, cmpOp), List(gen.mkAttributedQualifier(targ.tpe)))
                      }
                    case RefinedType(parents, decls) if (parents.length >= 2) =>
                      gen.evalOnce(qual, currentOwner, unit) { q =>
                        atPos(tree.pos) {
                          parents map mkIsInstanceOf(q) reduceRight gen.mkAnd
                        }
                      }
                    case _ =>
                      if (isSeqClass(targ.tpe.symbol)) {
                        atPos(tree.pos) {
                          gen.evalOnce(qual, currentOwner, unit) { q =>
                            gen.mkOr(
                              mkIsInstanceOf(q)(targ.tpe),
                              atPos(tree.pos) {
                                Apply(gen.mkAttributedRef(isArrayMethod), List(q()))
                              })
                          }
                        }
                      } else tree
                  }
                case _ => tree
              }
            else fn match {
              case Select(qual, _) if (settings.Xexperimental.value &&
                                       fn.symbol.owner.isRefinementClass &&
                                       fn.symbol.allOverriddenSymbols.isEmpty) =>
                ApplyDynamic(qual, args) setSymbol fn.symbol setPos tree.pos
              case _ =>
                tree
            }

          case Select(_, _) =>
            if (tree.symbol.owner.isRefinementClass) {
              val overridden = tree.symbol.allOverriddenSymbols
              assert(!overridden.isEmpty, tree.symbol)
              tree.symbol = overridden.head
            }
            tree

          case Template(parents, body) =>
            assert(!currentOwner.isImplClass)
            //Console.println("checking no dble defs " + tree)//DEBUG
            checkNoDoubleDefs(tree.symbol.owner)
            copy.Template(tree, parents, addBridges(body, currentOwner))
          case _ =>
            tree
        }
        tree1 match {
          case EmptyTree | TypeTree() =>
            tree1 setType erasure(tree1.tpe)
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            val result = super.transform(tree1) setType null
            tpt.tpe = erasure(tree.symbol.tpe).resultType
            result
          case _ =>
            super.transform(tree1) setType null
        }
      }
    }

    /** The main transform function: Pretransfom the tree, and then
     *  re-type it at phase erasure.next.
     */
    override def transform(tree: Tree): Tree = {
      val tree1 = preTransformer.transform(tree)
      atPhase(phase.next) {
        val tree2 = mixinTransformer.transform(tree1)
        if (settings.debug.value) log("tree after addinterfaces: \n" + tree2)
        newTyper(rootContext(unit, tree, true)).typed(tree2)
      }
    }
  }
}
