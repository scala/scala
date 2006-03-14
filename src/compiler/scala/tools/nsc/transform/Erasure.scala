/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.transform;

import collection.mutable.HashMap;
import symtab._;
import Flags._;
import scala.tools.nsc.util.Position;
import scala.collection.mutable.ListBuffer;

abstract class Erasure extends AddInterfaces with typechecker.Analyzer {
  import global._;                  // the global environment
  import definitions._;             // standard classes and methods
  import typer.{typed};             // methods to type trees
  import posAssigner.atPos;         // for filling in tree positions

  val phaseName: String = "erasure";
  def newTransformer(unit: CompilationUnit): Transformer = new ErasureTransformer(unit);

// -------- erasure on types --------------------------------------------------------

  /** The erasure |T| of a type T. This is:
   *   - For a constant type, itself.
   *   - For a type-bounds structure, the erasure of its upper bound.
   *   - For every other singleton type, the erasure of its supertype.
   *   - For a typeref scala.Array[T] where T is an abstract type, scala.runtime.BoxedArray.
   *   - For a typeref scala.Array[T] where T is not an abstract type, scala.Array[|T|].
   *   - For a typeref scala.Any or scala.AnyVal, java.lang.Object.
   *   - For a typeref scala.Unit, scala.runtime.BoxedUnit.
   *   - For a typeref P.C[Ts] where C refers to a class, |P|.C.
   *   - For a typeref P.C[Ts] where C refers to an alias type, the erasure of C's alias.
   *   - For a typeref P.C[Ts] where C refers to an abstract type, the erasure of C's upper bound.
   *   - For a non-empty type intersection (possibly with refinement), the erasure of its first parent.
   *   - For an empty type intersection, java.lang.Object
   *   - For a method type (Fs)scala.Unit, (|Fs|)scala#Unit.
   *   - For any other method type (Fs)Y, (|Fs|)|T|.
   *   - For a polymorphic type, the erasure of its result type
   *   - For the class info type of java.lang.Object, the same type without any parents
   *   - For a class info type of a value class, the same type without any parents
   *   - For any other class info type with parents Ps, the same type with parents |Ps|, but
   *     with duplicate references of Object removed.
   *   - for all other types, the type itself (with any sub-components erased)
   */
  private val erasure = new TypeMap {
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
	  if (restpe.symbol == UnitClass) erasedTypeRef(UnitClass) else apply(restpe));
      case RefinedType(parents, decls) =>
	if (parents.isEmpty) erasedTypeRef(ObjectClass)
	else apply(parents.head)
      case ClassInfoType(parents, decls, clazz) =>
	ClassInfoType(
	  if ((clazz == ObjectClass) || (isValueClass(clazz))) List()
	  else if (clazz == ArrayClass) List(erasedTypeRef(ObjectClass))
	  else removeDoubleObject(parents map this),
          decls, clazz)
      case _ =>
	mapOver(tp)
    }
  }

  /** Type reference after erasure */
  def erasedTypeRef(sym: Symbol): Type = typeRef(erasure(sym.owner.tpe), sym, List());

  /** Remove duplicate references to class Object in a list of parent classes
   * todo: needed?
   */
  private def removeDoubleObject(tps: List[Type]): List[Type] = tps match {
    case List() => List()
    case tp :: tps1 =>
      if (tp.symbol == ObjectClass) tp :: tps1.filter(.symbol.!=(ObjectClass))
      else tp :: removeDoubleObject(tps1)
  }

  /** The symbol's erased info. This is the type's erasure, except for the following symbols
   *   - For $asInstanceOf    : [T]T
   *   - For $isInstanceOf    : [T]scala#Boolean
   *   - For class Array      : [T]C where C is the erased classinfo of the Array class
   *   - For Array[T].<init>  : {scala#Int)Array[T]
   *   - For a type parameter : A type bounds type consisting of the erasures of its bounds.
   */
  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym == Object_asInstanceOf)
      sym.info
    else if (sym == Object_isInstanceOf || sym == ArrayClass)
      PolyType(sym.info.typeParams, erasure(sym.info.resultType))
    else if (sym.isAbstractType)
      TypeBounds(WildcardType, WildcardType)
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

// -------- boxing/unboxing --------------------------------------------------------

  override def newTyper(context: Context) = new Eraser(context);

  /** The modifier typer which retypes with erased types. */
  class Eraser(context: Context) extends Typer(context) {

    private def evalOnce(expr: Tree, within: (() => Tree) => Tree): Tree =
      if (treeInfo.isPureExpr(expr)) {
	within(() => expr);
      } else {
	val temp = context.owner.newValue(expr.pos, context.unit.fresh.newName())
	  .setFlag(SYNTHETIC).setInfo(expr.tpe);
	Block(List(ValDef(temp, expr)), within(() => Ident(temp) setType expr.tpe))
      }

    /** Box `tree' of unboxed type */
    private def box(tree: Tree): Tree =
      typed {
	atPos(tree.pos) {
          val sym = tree.tpe.symbol;
	  if (sym == UnitClass) {
            if (treeInfo.isPureExpr(tree)) gen.mkRef(BoxedUnit_UNIT)
            else Block(List(tree), gen.mkRef(BoxedUnit_UNIT))
          } else if (sym == ArrayClass) {
	    val elemClass = tree.tpe.typeArgs.head.symbol;
	    val boxedClass = if (isValueClass(elemClass)) boxedArrayClass(elemClass)
			     else BoxedObjectArrayClass;
            Apply(Select(New(TypeTree(boxedClass.tpe)), nme.CONSTRUCTOR), List(tree))
          } else {
            val boxedModule = boxedClass(tree.tpe.symbol).linkedModule;
            Apply(Select(gen.mkRef(boxedModule), nme.box), List(tree))
	  }
        }
      }

    /** generate  ScalaRuntime.boxArray(tree) */
    private def boxArray(tree: Tree): Tree =
      typed {
        atPos(tree.pos) {
          runtimeCall(nme.boxArray, List(tree))
        }
      }

    /** The method-name xxxValue, where Xxx is a numeric value class name */
    def unboxOp(tp: Type): Name = {
      val clazzName = tp.symbol.name.toString();
      newTermName(
        String.valueOf((clazzName.charAt(0) + ('a' - 'A')).asInstanceOf[char]) +
        clazzName.substring(1) + "Value")
    }

    private def runtimeCall(meth: Name, args: List[Tree]): Tree =
      Apply(Select(gen.mkRef(ScalaRunTimeModule), meth), args);

    /** Unbox `tree' of boxed type to expected type `pt' */
    private def unbox(tree: Tree, pt: Type): Tree =
      typed {
        atPos(tree.pos) {
          if (pt.symbol == UnitClass) {
            if (treeInfo.isPureExpr(tree)) Literal(())
	    else Block(List(tree), Literal(()))
          } else {
            if (pt.symbol == BooleanClass) {
              val tree1 = adaptToType(tree, boxedClass(BooleanClass).tpe);
              runtimeCall(nme.booleanValue, List(tree1))
            } else if (pt.symbol == ArrayClass) {
              val tree1 = adaptToType(tree, BoxedArrayClass.tpe);
              val elemClass = pt.typeArgs.head.symbol;
              val elemTag =
                if (isValueClass(elemClass))
                  runtimeCall(newTermName(elemClass.name.toString() + "Tag"), List())
                else
                  Literal(signature(pt.typeArgs.head));
              //System.out.println("unboxing " + tree + ":" + tree.tpe + " to " + pt);//DEBUG
              runtimeCall(nme.arrayValue, List(tree1, elemTag))
            } else {
	      assert(isNumericValueClass(pt.symbol));
              val tree1 = adaptToType(tree, BoxedNumberClass.tpe);
              runtimeCall(unboxOp(pt), List(tree1))
            }
          }
        }
      }

    private def cast(tree: Tree, pt: Type): Tree =
      if (pt.symbol == ArrayClass && tree.tpe.symbol == ObjectClass)
	typed {
	  atPos(tree.pos) {
	    evalOnce(tree, x =>
	      gen.cast(
		If(
		  Apply(
		    TypeApply(
		      Select(x(), Object_isInstanceOf),
		      List(TypeTree(BoxedArrayClass.tpe))),
		    List()),
		  unbox(gen.cast(x(), BoxedArrayClass.tpe), pt),
		  x()),
		pt))
	  }
	}
      else if (pt.symbol.isSubClass(BoxedArrayClass) && tree.tpe.symbol == ObjectClass)
        typed {
	  atPos(tree.pos) {
	    evalOnce(tree, x =>
	      gen.cast(
		If(
		  Apply(
		    TypeApply(
		      Select(x(), Object_isInstanceOf),
		      List(TypeTree(BoxedArrayClass.tpe))),
		    List()),
                  x(),
                  boxArray(x())),
		pt))
	  }
        }
      else gen.cast(tree, pt);

    /** Is symbol a member of unboxed arrays (which will be expanded directly later)? */
    private def isUnboxedArrayMember(sym: Symbol) = (
      sym.name == nme.apply || sym.name == nme.length || sym.name == nme.update ||
      sym.owner == ObjectClass
    );

    /** Is symbol a member of a boxed value class (which will not be expanded later)? */
    def isBoxedValueMember(sym: Symbol) =
      (sym.name == nme.equals_ || sym.name == nme.hashCode_ || sym.name == nme.toString_ ||
       (sym.name == nme.EQ || sym.name == nme.NE) && sym.info.paramTypes.head.symbol == ObjectClass ||
       sym == Object_isInstanceOf || sym == Object_asInstanceOf);

    /** Adapt `tree' to expected type `pt' */
    private def adaptToType(tree: Tree, pt: Type): Tree = {
      if (settings.debug.value && pt != WildcardType) log("adapting " + tree + ":" + tree.tpe + " to " + pt);//debug
      if (tree.tpe <:< pt)
        tree
      else if (isUnboxedClass(tree.tpe.symbol) && !isUnboxedClass(pt.symbol))
        adaptToType(box(tree), pt)
      else if (tree.tpe.isInstanceOf[MethodType] && tree.tpe.paramTypes.isEmpty) {
        assert(tree.symbol.isStable);
        adaptToType(Apply(tree, List()) setPos tree.pos setType tree.tpe.resultType, pt)
      } else if (pt <:< tree.tpe)
        cast(tree, pt)
      else if (isUnboxedClass(pt.symbol) && !isUnboxedClass(tree.tpe.symbol))
        adaptToType(unbox(tree, pt), pt)
      else
        cast(tree, pt)
    }

    /** Replace member references as follows:
     *  - `x == y'  for `==' in class Any becomes `x equals y' with `equals' in class Object
     *  - `x != y'  for `!=' in class Any becomes `!(x equals y)' with `equals' in class Object
     *  - `new BoxedArray.<init>(len)' becomes `new BoxedAnyArray.<init>(len): BoxedArray'
     *    (the widening typing is necessary so that subsequent member symbols stay the same)
     *  - `x.asInstanceOf[T]' and `x.asInstanceOf$erased[T]' become `x.$asInstanceOf[T]'
     *  - `x.isInstanceOf[T]' and `x.isInstanceOf$erased[T]' become `x.$isInstanceOf[T]'
     *  - `x.m' where `m' is some other member of Any becomes `x.m' where m is a member of class Object
     *  - `x.m' where `x' has unboxed value type `T' and `m' is not a directly
     *    translated member of `T' becomes T.box(x).m
     *  - `x.m' where `x' has type `Array[T]' and `m' is not a directly
     *    translated member of `Array' becomes new BoxedTArray.<init>(x).m
     *  - `x.m' where `x' is a reference type and `m' is a directly translated member of value type
     *    T becomes x.TValue().m
     *  - All forms of `x.m' where `x' is a boxed type and `m' is a member of an unboxed class
     *    become `x.m' where `m' is the corresponding member of the boxed class.
     */
    private def adaptMember(tree: Tree): Tree = {
      tree match {
        case Apply(Select(New(tpt), name), args) if (tpt.tpe.symbol == BoxedArrayClass) =>
          assert(name == nme.CONSTRUCTOR);
          atPos(tree.pos) {
            Typed(Apply(Select(New(TypeTree(BoxedAnyArrayClass.tpe)), name), args), tpt)
          }
	case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List())
	if ((tree.symbol == Any_asInstanceOf || tree.symbol == Any_asInstanceOfErased)) =>
	  val qual1 = typedQualifier(qual);
	  val targClass = targ.tpe.symbol;
	  val qualClass = qual1.tpe.symbol;
	  if (isNumericValueClass(qualClass) && isNumericValueClass(targClass))
	    // convert numeric type casts
	    atPos(tree.pos)(Apply(Select(qual1, "to" + targClass.name), List()))
	  else if (isValueClass(targClass) ||
		   (targClass == ArrayClass && (qualClass isSubClass BoxedArrayClass)))
	    unbox(qual1, targ.tpe)
	  else if (targClass == ArrayClass && qualClass == ObjectClass)
            cast(qual1, targ.tpe)
	  else
            tree
        case Select(qual, name) if (name != nme.CONSTRUCTOR) =>
          if (tree.symbol == Any_asInstanceOf || tree.symbol == Any_asInstanceOfErased)
            adaptMember(atPos(tree.pos)(Select(qual, Object_asInstanceOf)))
          else if (tree.symbol == Any_isInstanceOf || tree.symbol == Any_isInstanceOfErased)
            adaptMember(atPos(tree.pos)(Select(qual, Object_isInstanceOf)))
          else if (tree.symbol != NoSymbol && tree.symbol.owner == AnyClass)
            adaptMember(atPos(tree.pos)(Select(qual, getMember(ObjectClass, name))))
          else {
            var qual1 = typedQualifier(qual);
            if ((isValueClass(qual1.tpe.symbol) && isBoxedValueMember(tree.symbol)) ||
                (qual1.tpe.symbol == ArrayClass && !isUnboxedArrayMember(tree.symbol))) {
              qual1 = box(qual1);
            } else if (!isValueClass(qual1.tpe.symbol) &&
                       tree.symbol != NoSymbol &&
                       isValueClass(tree.symbol.owner) &&
                       !isBoxedValueMember(tree.symbol)) {
              qual1 = unbox(qual1, tree.symbol.owner.tpe)
            }
            if (tree.symbol != NoSymbol)
              if (isUnboxedClass(tree.symbol.owner) && !isUnboxedClass(qual1.tpe.symbol))
                tree.symbol = NoSymbol
              else if (qual1.tpe.isInstanceOf[MethodType] && qual1.tpe.paramTypes.isEmpty) {
                assert(qual1.symbol.isStable);
                qual1 = Apply(qual1, List()) setPos qual1.pos setType qual1.tpe.resultType;
              } else if (!(qual1.isInstanceOf[Super] || (qual1.tpe.symbol isSubClass tree.symbol.owner)))
                qual1 = cast(qual1, tree.symbol.owner.tpe);
            copy.Select(tree, qual1, name)
          }
        case _ =>
          tree
      }
    }

    /** A replacement for the standard typer's `adapt' method */
    override protected def adapt(tree: Tree, mode: int, pt: Type): Tree = adaptToType(tree, pt);

    /** A replacement for the standard typer's `typed1' method */
    override protected def typed1(tree: Tree, mode: int, pt: Type): Tree = {
      val tree1 = super.typed1(adaptMember(tree), mode, pt);
      def adaptCase(cdef: CaseDef): CaseDef = {
	val body1 = adaptToType(cdef.body, tree1.tpe);
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
	case _ =>
	  tree1
      }
    }
  }

  /** The erasure transformer */
  class ErasureTransformer(unit: CompilationUnit) extends Transformer {

    /** Emit an error if there is a double definition. This can happen in the following
     *  circumstances:
     *   - A template defines two members with the same name and erased type.
     *   - A template defines and inherits two members `m' with different types,
     *     but their erased types are the same.
     *   - A template inherits two members `m' with different types,
     *     but their erased types are the same.
     */
    private def checkNoDoubleDefs(root: Symbol): unit = {
      def doubleDefError(sym1: Symbol, sym2: Symbol) = {
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
      }

      val decls = root.info.decls;
      var e = decls.elems;
      while (e != null) {
        if (e.sym.isTerm && !e.sym.isConstructor) {
	  var e1 = decls.lookupNextEntry(e);
	  while (e1 != null) {
	    if (atPhase(phase.next)(e1.sym.info =:= e.sym.info)) doubleDefError(e.sym, e1.sym);
	    e1 = decls.lookupNextEntry(e1)
	  }
        }
        e = e.next
      }

      val opc = new overridingPairs.Cursor(root) {
        override def exclude(sym: Symbol): boolean =
          !sym.isTerm || (sym hasFlag (PRIVATE | BRIDGE)) || super.exclude(sym);
	override def matches(sym1: Symbol, sym2: Symbol): boolean =
	  atPhase(phase.next)(sym1.tpe =:= sym2.tpe)
      }
      while (opc.hasNext) {
        if (!atPhase(currentRun.refchecksPhase.next)(
              root.thisType.memberType(opc.overriding) matches
	      root.thisType.memberType(opc.overridden))) {
	  if (settings.debug.value) log("" + opc.overriding.locationString + " " + opc.overriding.infosString + opc.overridden.locationString + " " + opc.overridden.infosString);
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

    /** Add bridge definitions to a template. This means:
     *  If there is a concrete member `m' which overrides a member in a base class of the template,
     *  and the erased types of the two members differ,
     *  and the two members are not inherited or defined by some parent class of the template,
     *  then a bridge from the overridden member `m1' to the member `m0' is added.
     *  The bridge has the erased type of `m1' and forwards to `m0'.
     *  No bridge is added if there is already a bridge to `m0' with the erased type of `m1'
     *  in the template.
     */
    private def bridgeDefs(owner: Symbol): List[Tree] = {
      //System.out.println("computing bridges for " + owner);//DEBUG
      val site = owner.thisType;
      val bridgesScope = new Scope();
      val bridgeTarget = new HashMap[Symbol, Symbol];
      var bridges: List[Tree] = List();
      val opc = atPhase(phase.prev) { 	// to avoid DEFERRED flags for interfaces
	new overridingPairs.Cursor(owner) {
          override def parents: List[Type] = List(owner.info.parents.head);
          override def exclude(sym: Symbol): boolean =
            !sym.isMethod || (sym hasFlag (PRIVATE | BRIDGE)) || super.exclude(sym);
        }
      }
      while (opc.hasNext) {
	val member = opc.overriding;
	val other = opc.overridden;
        //System.out.println("bridge? " + member + ":" + member.tpe + member.locationString + " to " + other + ":" + other.tpe + other.locationString);//DEBUG
	if (!atPhase(phase.prev)(member hasFlag DEFERRED)) {
          val otpe = erasure(other.tpe);
	  val bridgeNeeded = atPhase(phase.next) (
	    !(other.tpe =:= member.tpe) &&
            !(deconstMap(other.tpe) =:= deconstMap(member.tpe)) &&
            { var e = bridgesScope.lookupEntry(member.name);
              while (e != null && !((e.sym.tpe =:= otpe) && (bridgeTarget(e.sym) == member)))
		e = bridgesScope.lookupNextEntry(e);
              e == null
            }
	  );
	  if (bridgeNeeded) {
            val bridge = other.cloneSymbolImpl(owner)
              .setPos(owner.pos)
              .setFlag(member.flags | BRIDGE)
              .resetFlag(ACCESSOR | DEFERRED | lateDEFERRED)
              .setInfo(otpe);
            bridgeTarget(bridge) = member;
	    owner.info.decls.enter(bridge);
            bridgesScope enter bridge;
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
        val bridges = bridgeDefs(base);
        if (bridges.isEmpty) stats else stats ::: bridges
      }

    /** Transform tree at phase `erasure' before retyping it. This entails the following:
     *    - Remove all type parameters in class and method definitions.
     *    - Remove all abstract and alias type definitions.
     *    - Remove all type applications other than those involving a type test or cast.
     *    - Remove all empty trees in statements and definitions in a PackageDef.
     *    - Check that there are no double definitions in a template.
     *    - Add bridge definitions to a template.
     *    - Replace all types in type nodes and the EmptyTree object by their erasure.
     *      Type nodes of type Unit representing result types of methods are left alone.
     *    - Reset all other type attributes to `null, thus enforcing a retyping.
     */
    private val preTransformer = new Transformer {
      override def transform(tree: Tree): Tree = {
        if (tree.symbol == ArrayClass) return tree;
        val tree1 = tree match {
          case ClassDef(mods, name, tparams, tpt, impl) =>
            if (settings.debug.value) log("defs of " + tree.symbol + " = " + tree.symbol.info.decls);
            copy.ClassDef(tree, mods, name, List(), tpt, impl)
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            copy.DefDef(tree, mods, name, List(), vparamss, tpt, rhs)
	  case AbsTypeDef(_, _, _, _) =>
	    EmptyTree
	  case AliasTypeDef(_, _, _, _) =>
	    EmptyTree
          case TypeApply(fun, args) if (fun.symbol.owner != AnyClass) =>
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
            tree

          case Template(parents, body) =>
            assert(!currentOwner.isImplClass);
	    //System.out.println("checking no dble defs " + tree);//DEBUG
            checkNoDoubleDefs(tree.symbol.owner);
            copy.Template(tree, parents, addBridges(body, currentOwner));
          case _ =>
            tree
        }
        tree1 match {
          case EmptyTree | TypeTree() =>
            tree1 setType erasure(tree1.tpe)
	  case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
	    val result = super.transform(tree1) setType null;
	    tpt.tpe = erasure(tree.symbol.tpe).resultType;
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
      val tree1 = preTransformer.transform(tree);
      atPhase(phase.next) {
        val tree2 = mixinTransformer.transform(tree1);
        if (settings.debug.value) log("tree after addinterfaces: \n" + tree2);
        newTyper(startContext.make(
	  unit, tree, startContext.owner, startContext.scope, startContext.imports))
	  .typed(tree2)
      }
    }
  }
}
