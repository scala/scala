/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc
package transform

import scala.tools.nsc.symtab.classfile.ClassfileConstants._
import scala.collection.mutable.{HashMap,ListBuffer}
import scala.collection.immutable.Set
import scala.tools.nsc.util.Position
import symtab._
import Flags._

abstract class Erasure extends AddInterfaces with typechecker.Analyzer with ast.TreeDSL
{
  import global._                  // the global environment
  import definitions._             // standard classes and methods
  // @S: XXX: why is this here? earsure is a typer, if you comment this
  //          out erasure still works, uses its own typed methods.
  lazy val typerXXX = this.typer
  import typerXXX.{typed}             // methods to type trees

  import CODE._
  def typedPos(pos: Position)(tree: Tree) = typed { atPos(pos)(tree) }

  val phaseName: String = "erasure"

  def newTransformer(unit: CompilationUnit): Transformer =
    new ErasureTransformer(unit)

// -------- erasure on types --------------------------------------------------------

  /** An extractor objec for generic arrays */
  object GenericArray {

    /** Is `tp` an unbounded generic type (i.e. which could be instantiated
     *  with primitive as well as class types)?.
     */
    private def genericCore(tp: Type): Type = tp match {
      case TypeRef(_, argsym, _) if (argsym.isAbstractType && !(argsym.owner hasFlag JAVA)) =>
        tp
      case ExistentialType(tparams, restp) =>
        genericCore(restp)
      case _ =>
        NoType
    }

    /** If `tp` is of the form Array[...Array[T]...] where `T` is an abstract type
     *  then Some(N, T) where N is the number of Array constructors enclosing `T`,
     *  otherwise None. Existentials on any level are ignored.
     */
    def unapply(tp: Type): Option[(Int, Type)] = tp match {
      case TypeRef(_, ArrayClass, List(arg)) =>
        genericCore(arg) match {
          case NoType =>
            unapply(arg) match {
              case Some((level, core)) => Some((level + 1, core))
              case None => None
            }
          case core =>
            Some(1, core)
        }
      case ExistentialType(tparams, restp) =>
        unapply(restp)
      case _ =>
        None
    }
  }

  private def unboundedGenericArrayLevel(tp: Type): Int = tp match {
    case GenericArray(level, core) if !(core <:< AnyRefClass.tpe) => level
    case _ => 0
  }

  /** <p>
   *    The erasure <code>|T|</code> of a type <code>T</code>. This is:
   *  </p>
   *  <ul>
   *    <li>For a constant type, itself.</li>
   *    <li>For a type-bounds structure, the erasure of its upper bound.</li>
   *    <li>For every other singleton type, the erasure of its supertype.</li>
   *    <li>
   *      For a typeref <code>scala.Array+[T]</code> where <code>T</code> is
   *      an abstract type, <code>AnyRef</code>.
   *    </li>
   *    <li>
   *   - For a typeref scala.Array+[T] where T is not an abstract type, scala.Array+[|T|].
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
    def apply(tp: Type): Type = {
      tp match {
        case ConstantType(_) =>
          tp
        case st: SubType =>
          apply(st.supertype)
        case TypeRef(pre, sym, args) =>
          if (sym == ArrayClass)
            if (unboundedGenericArrayLevel(tp) == 1) ObjectClass.tpe
            else if (args.head.typeSymbol == NothingClass || args.head.typeSymbol == NullClass) arrayType(ObjectClass.tpe)
            else typeRef(apply(pre), sym, args map this)
          else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass) erasedTypeRef(ObjectClass)
          else if (sym == UnitClass) erasedTypeRef(BoxedUnitClass)
          else if (sym.isClass)
            typeRef(apply(if (sym.owner.isClass) sym.owner.tpe else pre), sym, List())
          else apply(sym.info)
        case PolyType(tparams, restpe) =>
          apply(restpe)
        case ExistentialType(tparams, restpe) =>
          apply(restpe)
        case mt @ MethodType(params, restpe) =>
          MethodType(
            cloneSymbols(params) map (p => p.setInfo(apply(p.tpe))),
            if (restpe.typeSymbol == UnitClass)
              erasedTypeRef(UnitClass)
            else if (settings.Xexperimental.value)
              apply(mt.resultType(params map (_.tpe))) // this gets rid of DeBruijnTypes
            else
              apply(restpe))
        case RefinedType(parents, decls) =>
          if (parents.isEmpty) erasedTypeRef(ObjectClass)
          else apply(parents.head)
        case AnnotatedType(_, atp, _) =>
          apply(atp)
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
  }

  private object NeedsSigCollector extends TypeCollector(false) {
    def traverse(tp: Type) {
      if (!result) {
        tp match {
          case st: SubType =>
            traverse(st.supertype)
          case TypeRef(pre, sym, args) =>
            if (sym == ArrayClass) args foreach traverse
            else if (sym.isTypeParameterOrSkolem || sym.isExistential || !args.isEmpty) result = true
            else if (!sym.owner.isPackageClass) traverse(pre)
          case PolyType(_, _) | ExistentialType(_, _) =>
            result = true
          case RefinedType(parents, decls) =>
            if (!parents.isEmpty) traverse(parents.head)
          case ClassInfoType(parents, _, _) =>
            parents foreach traverse
	  case AnnotatedType(_, atp, _) =>
	    traverse(atp)
          case _ =>
            mapOver(tp)
        }
      }
    }
  }

  private def needsJavaSig(tp: Type) = !settings.Ynogenericsig.value && NeedsSigCollector.collect(tp)

  private lazy val tagOfClass = Map[Symbol,Char](
    ByteClass -> BYTE_TAG,
    CharClass -> CHAR_TAG,
    DoubleClass -> DOUBLE_TAG,
    FloatClass -> FLOAT_TAG,
    IntClass -> INT_TAG,
    LongClass -> LONG_TAG,
    ShortClass -> SHORT_TAG,
    BooleanClass -> BOOL_TAG,
    UnitClass -> VOID_TAG
  )

  /** The Java signature of type 'info', for symbol sym. The symbol is used to give the right return
   *  type for constructors.
   */
  def javaSig(sym: Symbol, info: Type): Option[String] = atPhase(currentRun.erasurePhase) {

    def jsig(tp: Type): String = jsig2(false, List(), tp)

    def jsig2(toplevel: Boolean, tparams: List[Symbol], tp0: Type): String = {
      val tp = tp0.dealias
      tp match {
        case st: SubType =>
          jsig2(toplevel, tparams, st.supertype)
        case ExistentialType(tparams, tpe) =>
          jsig2(toplevel, tparams, tpe)
        case TypeRef(pre, sym, args) =>
          def argSig(tp: Type) =
            if (tparams contains tp.typeSymbol) {
              val bounds = tp.typeSymbol.info.bounds
              if (!(AnyRefClass.tpe <:< bounds.hi)) "+"+jsig(bounds.hi)
              else if (!(bounds.lo <:< NullClass.tpe)) "-"+jsig(bounds.lo)
              else "*"
            } else if (tp.typeSymbol == UnitClass) {
              jsig(ObjectClass.tpe)
            } else {
              boxedClass get tp.typeSymbol match {
                case Some(boxed) => jsig(boxed.tpe)
                case None => jsig(tp)
              }
            }
          def classSig: String =
            "L"+atPhase(currentRun.icodePhase)(sym.fullNameString + global.genJVM.moduleSuffix(sym)).replace('.', '/')
          def classSigSuffix: String =
            "."+sym.name
          if (sym == ArrayClass)
            ARRAY_TAG.toString+(args map jsig).mkString
          else if (sym.isTypeParameterOrSkolem && !sym.owner.isTypeParameterOrSkolem /*not a higher-order type parameter, as these are suppressed*/)
            TVAR_TAG.toString+sym.name+";"
          else if (sym == AnyClass || sym == AnyValClass || sym == SingletonClass)
            jsig(ObjectClass.tpe)
          else if (sym == UnitClass)
            jsig(BoxedUnitClass.tpe)
          else if (sym == NothingClass)
            jsig(RuntimeNothingClass.tpe)
          else if (sym == NullClass)
            jsig(RuntimeNullClass.tpe)
          else if (isValueClass(sym))
            tagOfClass(sym).toString
          else if (sym.isClass)
            {
              if (needsJavaSig(pre)) {
                val s = jsig(pre)
                if (s.charAt(0) == 'L') s.substring(0, s.length - 1) + classSigSuffix
                else classSig
              } else classSig
            } + {
              if (args.isEmpty) "" else "<"+(args map argSig).mkString+">"
            } + ";"
          else jsig(erasure(tp))
        case PolyType(tparams, restpe) =>
          def hiBounds(bounds: TypeBounds): List[Type] = bounds.hi.normalize match {
            case RefinedType(parents, _) => parents map normalize
            case tp => List(tp)
          }
          def boundSig(bounds: List[Type]) = {
            def isClassBound(t: Type) = !t.typeSymbol.isTrait
            val classBound = bounds find isClassBound match {
              case Some(t) => jsig(t)
              case None => ""
            }
            ":"+classBound+(for (t <- bounds if !isClassBound(t)) yield ":"+jsig(t)).mkString
          }
          assert(!tparams.isEmpty)
          def paramSig(tsym: Symbol) = tsym.name+boundSig(hiBounds(tsym.info.bounds))
          (if (toplevel) "<"+(tparams map paramSig).mkString+">" else "")+jsig(restpe)
        case MethodType(params, restpe) =>
          "("+(params map (_.tpe) map jsig).mkString+")"+
          (if (restpe.typeSymbol == UnitClass || sym.isConstructor) VOID_TAG.toString else jsig(restpe))
        case RefinedType(parents, decls) if (!parents.isEmpty) =>
          jsig(parents.head)
        case ClassInfoType(parents, _, _) =>
          (parents map jsig).mkString
        case AnnotatedType(_, atp, _) =>
          jsig(atp)
        case _ =>
          val etp = erasure(tp)
          if (etp eq tp) throw new UnknownSig
          else jsig(etp)
      }
    }
    if (needsJavaSig(info)) {
      try {
        //println("Java sig of "+sym+" is "+jsig2(true, List(), sym.info))//DEBUG
        Some(jsig2(true, List(), info))
      } catch {
        case ex: UnknownSig => None
      }
    }
    else None
  }

  class UnknownSig extends Exception

  /** Type reference after erasure */
  def erasedTypeRef(sym: Symbol): Type =
    typeRef(erasure(sym.owner.tpe), sym, List())

  /** Remove duplicate references to class Object in a list of parent classes
   * todo: needed?
   */
  private def removeDoubleObject(tps: List[Type]): List[Type] = tps match {
    case List() => List()
    case tp :: tps1 =>
      if (tp.typeSymbol == ObjectClass) tp :: tps1.filter(_.typeSymbol != ObjectClass)
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
          case MethodType(params, TypeRef(pre, sym, args)) =>
            MethodType(cloneSymbols(params) map (p => p.setInfo(erasure(p.tpe))),
                       typeRef(erasure(pre), sym, args))
        }
      else if (sym.name == nme.apply)
        tp
      else if (sym.name == nme.update)
        tp match {
          case MethodType(List(index, tvar), restpe) =>
            MethodType(List(index.cloneSymbol.setInfo(erasure(index.tpe)), tvar),
                       erasedTypeRef(UnitClass))
        }
      else erasure(tp)
    } else if (
      sym.owner != NoSymbol &&
      sym.owner.owner == ArrayClass &&
      sym == Array_update.paramss.head(1)) {
      // special case for Array.update: the non-erased type remains, i.e. (Int,A)Unit
      // since the erasure type map gets applied to every symbol, we have to catch the
      // symbol here
      tp
    } else {
/*
      val erased =
        if (sym.isGetter && sym.tpe.isInstanceOf[MethodType])
          erasure mapOver sym.tpe // for getters, unlike for normal methods, always convert Unit to BoxedUnit.
        else
          erasure(tp)
*/
      transformMixinInfo(erasure(tp))
    }

  val deconstMap = new TypeMap {
   def apply(tp: Type): Type = tp match {
     case PolyType(_, _) => mapOver(tp)
     case MethodType(_, _) => mapOver(tp)
     case _ => tp.deconst
   }
  }

  /** The symbol which is called by a bridge;
   *  @pre phase > erasure
   */
  def bridgedSym(bridge: Symbol) =
    bridge.owner.info.nonPrivateDecl(bridge.name) suchThat {
      sym => !(sym hasFlag BRIDGE) &&
             matchesType(sym.tpe, bridge.tpe, true) &&
             sym.tpe.resultType <:< bridge.tpe.resultType
    }

// -------- erasure on trees ------------------------------------------

  override def newTyper(context: Context) = new Eraser(context)

  /** The modifier typer which retypes with erased types. */
  class Eraser(context: Context) extends Typer(context) {

    /** Box `tree' of unboxed type */
    private def box(tree: Tree): Tree = tree match {
      case LabelDef(name, params, rhs) =>
        val rhs1 = box(rhs)
        treeCopy.LabelDef(tree, name, params, rhs1) setType rhs1.tpe
      case _ =>
        typedPos(tree.pos)(tree.tpe.typeSymbol match {
          case UnitClass  =>
            if (treeInfo isPureExpr tree) REF(BoxedUnit_UNIT)
            else BLOCK(tree, REF(BoxedUnit_UNIT))
          case x          =>
            assert(x != ArrayClass)
            (REF(boxMethod(x)) APPLY tree) setPos (tree.pos) setType ObjectClass.tpe
        })
    }

    /** generate  ScalaRuntime.boxArray(tree)
     *  !!! todo: optimize this in case the runtime type is known
     */
    private def boxArray(tree: Tree): Tree = tree match {
      case LabelDef(name, params, rhs) =>
        val rhs1 = boxArray(rhs)
        treeCopy.LabelDef(tree, name, params, rhs1) setType rhs1.tpe
      case _ =>
        typedPos(tree.pos) { gen.mkRuntimeCall(nme.boxArray, List(tree)) }
    }

    /** Unbox <code>tree</code> of boxed type to expected type <code>pt</code>.
     *
     *  @param tree the given tree
     *  @param pt   the expected type.
     *  @return     the unboxed tree
     */
    private def unbox(tree: Tree, pt: Type): Tree = tree match {
      case LabelDef(name, params, rhs) =>
        val rhs1 = unbox(rhs, pt)
        treeCopy.LabelDef(tree, name, params, rhs1) setType rhs1.tpe
      case _ =>
        typedPos(tree.pos)(pt.typeSymbol match {
          case UnitClass  =>
            if (treeInfo isPureExpr tree) UNIT
            else BLOCK(tree, UNIT)
          case x          =>
            assert(x != ArrayClass)
            (REF(unboxMethod(pt.typeSymbol)) APPLY tree) setType pt
        })
    }

    /**   Generate a synthetic cast operation from <code>tree.tpe</code> to <code>pt</code>.
     */
    private def cast(tree: Tree, pt: Type): Tree =
      tree AS_ATTR pt

    /** Is symbol a member of unboxed arrays (which will be expanded directly
     *  later)?
     *
     *  @param sym ..
     *  @return    <code>true</code> if ..
     */
    private def isUnboxedArrayMember(sym: Symbol) = sym.name match {
      case nme.apply | nme.length | nme.update  => true
      case _                                    => sym.owner == ObjectClass
    }

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
      else if (isValueClass(tree.tpe.typeSymbol) && !isValueClass(pt.typeSymbol))
        adaptToType(box(tree), pt)
      else if (tree.tpe.isInstanceOf[MethodType] && tree.tpe.paramTypes.isEmpty) {
        if (!tree.symbol.isStable) assert(false, "adapt "+tree+":"+tree.tpe+" to "+pt)
        adaptToType(Apply(tree, List()) setPos tree.pos setType tree.tpe.resultType, pt)
      } else if (pt <:< tree.tpe)
        cast(tree, pt)
      else if (isValueClass(pt.typeSymbol) && !isValueClass(tree.tpe.typeSymbol))
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
        case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List()) if tree.symbol == Any_asInstanceOf =>
          val qual1 = typedQualifier(qual)
          val qualClass = qual1.tpe.typeSymbol
          val targClass = targ.tpe.typeSymbol
/*
          if (isNumericValueClass(qualClass) && isNumericValueClass(targClass))
            // convert numeric type casts
            atPos(tree.pos)(Apply(Select(qual1, "to" + targClass.name), List()))
          else
*/
          if (isValueClass(targClass)) unbox(qual1, targ.tpe)
          else tree
        case Select(qual, name) if (name != nme.CONSTRUCTOR) =>
          if (tree.symbol == NoSymbol)
            tree
          else if (tree.symbol == Any_asInstanceOf)
            adaptMember(atPos(tree.pos)(Select(qual, Object_asInstanceOf)))
          else if (tree.symbol == Any_isInstanceOf)
            adaptMember(atPos(tree.pos)(Select(qual, Object_isInstanceOf)))
          else if (tree.symbol.owner == AnyClass)
            adaptMember(atPos(tree.pos)(Select(qual, getMember(ObjectClass, name))))
          else {
            var qual1 = typedQualifier(qual);
            if ((isValueClass(qual1.tpe.typeSymbol) && !isUnboxedValueMember(tree.symbol)))
              qual1 = box(qual1)
            else if (!isValueClass(qual1.tpe.typeSymbol) && isUnboxedValueMember(tree.symbol))
              qual1 = unbox(qual1, tree.symbol.owner.tpe)

            if (isValueClass(tree.symbol.owner) && !isValueClass(qual1.tpe.typeSymbol))
              tree.symbol = NoSymbol
            else if (qual1.tpe.isInstanceOf[MethodType] && qual1.tpe.paramTypes.isEmpty) {
              assert(qual1.symbol.isStable, qual1.symbol);
              qual1 = Apply(qual1, List()) setPos qual1.pos setType qual1.tpe.resultType
            } else if (!(qual1.isInstanceOf[Super] || (qual1.tpe.typeSymbol isSubClass tree.symbol.owner))) {
              assert(tree.symbol.owner != ArrayClass)
              qual1 = cast(qual1, tree.symbol.owner.tpe)
            }
            treeCopy.Select(tree, qual1, name)
          }
        case SelectFromArray(qual, name, erasure) =>
          var qual1 = typedQualifier(qual)
          if (!(qual1.tpe <:< erasure)) qual1 = cast(qual1, erasure)
          Select(qual1, name) copyAttrs tree
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
    override protected def adapt(tree: Tree, mode: Int, pt: Type, original: Tree = EmptyTree): Tree =
      adaptToType(tree, pt)

    /** A replacement for the standard typer's `typed1' method */
    override protected def typed1(tree: Tree, mode: Int, pt: Type): Tree = {
      var tree1 = try {
        super.typed1(adaptMember(tree), mode, pt)
      } catch {
        case ex: Exception =>
          //if (settings.debug.value)
          Console.println("exception when typing " + tree);
          throw ex
        case er: TypeError =>
          Console.println("exception when typing " + tree)
          Console.println(er.msg + " in file " + context.owner.sourceFile)
          er.printStackTrace
          throw new Error
      }
      def adaptCase(cdef: CaseDef): CaseDef = {
        val body1 = adaptToType(cdef.body, tree1.tpe)
        treeCopy.CaseDef(cdef, cdef.pat, cdef.guard, body1) setType body1.tpe
      }
      def adaptBranch(branch: Tree): Tree =
        if (branch == EmptyTree) branch else adaptToType(branch, tree1.tpe);

      tree1 match {
        case If(cond, thenp, elsep) =>
          treeCopy.If(tree1, cond, adaptBranch(thenp), adaptBranch(elsep))
        case Match(selector, cases) =>
          treeCopy.Match(tree1, selector, cases map adaptCase)
        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree1, adaptBranch(block), catches map adaptCase, finalizer)
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
    private def checkNoDoubleDefs(root: Symbol) {
      def doubleDefError(sym1: Symbol, sym2: Symbol) {
        // the .toString must also be computed at the earlier phase
        def atRefc[T](op: => T) = atPhase[T](currentRun.refchecksPhase.next)(op)
        val tpe1 = atRefc(root.thisType.memberType(sym1))
        val tpe2 = atRefc(root.thisType.memberType(sym2))
        if (!tpe1.isErroneous && !tpe2.isErroneous)
          unit.error(
          if (sym1.owner == root) sym1.pos else root.pos,
          (if (sym1.owner == sym2.owner) "double definition:\n"
           else if (sym1.owner == root) "name clash between defined and inherited member:\n"
           else "name clash between inherited members:\n") +
          sym1 + ":" + atRefc(tpe1.toString) +
            (if (sym1.owner == root) "" else sym1.locationString) + " and\n" +
          sym2 + ":" + atRefc(tpe2.toString) +
            (if (sym2.owner == root) " at line " + (sym2.pos).line else sym2.locationString) +
          "\nhave same type" +
          (if (atRefc(tpe1 =:= tpe2)) "" else " after erasure: " + atPhase(phase.next)(sym1.tpe)))
        sym1.setInfo(ErrorType)
      }

      val decls = root.info.decls
      var e = decls.elems
      while (e ne null) {
        if (e.sym.isTerm) {
          var e1 = decls.lookupNextEntry(e)
          while (e1 ne null) {
            if (atPhase(phase.next)(e1.sym.info =:= e.sym.info)) doubleDefError(e.sym, e1.sym)
            e1 = decls.lookupNextEntry(e1)
          }
        }
        e = e.next
      }

      val opc = new overridingPairs.Cursor(root) {
        override def exclude(sym: Symbol): Boolean =
          (!sym.isTerm || sym.hasFlag(PRIVATE) || super.exclude(sym)
           // specialized members have no type history before 'specialize', causing duble def errors for curried defs
           || !sym.hasTypeAt(currentRun.refchecksPhase.id))

        override def matches(sym1: Symbol, sym2: Symbol): Boolean =
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
      for (bc <- root.info.baseClasses.tail; other <- bc.info.decls.toList) {
        if (other.isTerm && !other.isConstructor && !(other hasFlag (PRIVATE | BRIDGE))) {
          for (member <- root.info.nonPrivateMember(other.name).alternatives) {
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
    private def bridgeDefs(owner: Symbol): (List[Tree], Set[Symbol]) = {
      var toBeRemoved: Set[Symbol] = Set()
      //println("computing bridges for " + owner)//DEBUG
      assert(phase == currentRun.erasurePhase)
      val site = owner.thisType
      val bridgesScope = newScope
      val bridgeTarget = new HashMap[Symbol, Symbol]
      var bridges: List[Tree] = List()
      val opc = atPhase(currentRun.explicitOuterPhase) {
        new overridingPairs.Cursor(owner) {
          override def parents: List[Type] = List(owner.info.parents.head)
          override def exclude(sym: Symbol): Boolean =
            !sym.isMethod || sym.hasFlag(PRIVATE) || super.exclude(sym)
        }
      }
      while (opc.hasNext) {
        val member = opc.overriding
        val other = opc.overridden
        //Console.println("bridge? " + member + ":" + member.tpe + member.locationString + " to " + other + ":" + other.tpe + other.locationString);//DEBUG
        if (atPhase(currentRun.explicitOuterPhase)(!member.isDeferred)) {
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
              .resetFlag(ACCESSOR | DEFERRED | LAZY | lateDEFERRED)
            // the parameter symbols need to have the new owner
            bridge.setInfo(otpe.cloneInfo(bridge))
            bridgeTarget(bridge) = member
            atPhase(phase.next) { owner.info.decls.enter(bridge) }
            if (other.owner == owner) {
              //println("bridge to same: "+other+other.locationString)//DEBUG
              atPhase(phase.next) { owner.info.decls.unlink(other) }
              toBeRemoved += other
            }
            bridgesScope enter bridge
            bridges =
              atPhase(phase.next) {
                atPos(bridge.pos) {
                  val bridgeDef =
                    DefDef(bridge,
                      member.tpe match {
                        case MethodType(List(), ConstantType(c)) => Literal(c)
                        case _ =>
                          (((Select(This(owner), member): Tree) /: bridge.paramss)
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
      (bridges, toBeRemoved)
    }
/*
      for (bc <- site.baseClasses.tail; other <- bc.info.decls.toList) {
        if (other.isMethod && !other.isConstructor) {
          for (member <- site.nonPrivateMember(other.name).alternatives) {
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
        val (bridges, toBeRemoved) = bridgeDefs(base)
        if (bridges.isEmpty) stats
        else (stats filterNot (stat => toBeRemoved contains stat.symbol)) ::: bridges
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
        if (tree.symbol == ArrayClass && !tree.isType) return tree // !!! needed?
        val tree1 = tree match {
          case ClassDef(mods, name, tparams, impl) =>
            if (settings.debug.value)
              log("defs of " + tree.symbol + " = " + tree.symbol.info.decls)
            treeCopy.ClassDef(tree, mods, name, List(), impl)
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            treeCopy.DefDef(tree, mods, name, List(), vparamss, tpt, rhs)
          case TypeDef(_, _, _, _) =>
            EmptyTree
          case Apply(instanceOf @ TypeApply(fun @ Select(qual, name), args @ List(arg)), List()) // !!! todo: simplify by having GenericArray also extract trees
          if ((fun.symbol == Any_isInstanceOf || fun.symbol == Object_isInstanceOf) &&
              unboundedGenericArrayLevel(arg.tpe) > 0) =>
            val level = unboundedGenericArrayLevel(arg.tpe)
            def isArrayTest(arg: Tree) =
              gen.mkRuntimeCall("isArray", List(arg, Literal(Constant(level))))
            typedPos(tree.pos) {
              if (level == 1) isArrayTest(qual)
              else
                gen.evalOnce(qual, currentOwner, unit) { qual1 =>
                  gen.mkAnd(
                    Apply(TypeApply(Select(qual1(), fun.symbol),
                                    List(TypeTree(erasure(arg.tpe)))),
                          List()),
                    isArrayTest(qual1()))
                }
              }
          case TypeApply(fun, args) if (fun.symbol.owner != AnyClass &&
                                        fun.symbol != Object_asInstanceOf &&
                                        fun.symbol != Object_isInstanceOf) =>
            // leave all other type tests/type casts, remove all other type applications
            fun
          case Apply(fn @ Select(qual, name), args) if (fn.symbol.owner == ArrayClass) =>
            if (unboundedGenericArrayLevel(qual.tpe.widen) == 1)
              // convert calls to apply/update/length on generic arrays to
              // calls of ScalaRunTime.array_xxx method calls
              typedPos(tree.pos) { gen.mkRuntimeCall("array_"+name, qual :: args) }
            else
              // store exact array erasure in map to be retrieved later when we might
              // need to do the cast in adaptMember
              treeCopy.Apply(
                tree,
                SelectFromArray(qual, name, erasure(qual.tpe)).copyAttrs(fn),
                args)
          case Apply(fn, args) =>
            if (fn.symbol == Any_asInstanceOf)
              fn match {
                case TypeApply(Select(qual, _), List(targ)) =>
                  if (qual.tpe <:< targ.tpe) {
                    atPos(tree.pos) { Typed(qual, TypeTree(targ.tpe)) }
                  } else if (isNumericValueClass(qual.tpe.typeSymbol) &&
                             isNumericValueClass(targ.tpe.typeSymbol)) {
                    // convert numeric type casts
                    val cname = newTermName("to" + targ.tpe.typeSymbol.name)
                    val csym = qual.tpe.member(cname)
                    assert(csym != NoSymbol)
                    atPos(tree.pos) { Apply(Select(qual, csym), List()) }
                  } else
                    tree
              }
              // todo: also handle the case where the singleton type is buried in a compound
            else if (fn.symbol == Any_isInstanceOf)
              fn match {
                case TypeApply(sel @ Select(qual, name), List(targ)) =>
                  def mkIsInstanceOf(q: () => Tree)(tp: Type): Tree =
                    Apply(
                      TypeApply(
                        Select(q(), Object_isInstanceOf) setPos sel.pos,
                        List(TypeTree(tp) setPos targ.pos)) setPos fn.pos,
                      List()) setPos tree.pos
                  targ.tpe match {
                    case SingleType(_, _) | ThisType(_) | SuperType(_, _) =>
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
                      tree
                  }
                case _ => tree
              }
            else {
              def doDynamic(fn: Tree, qual: Tree): Tree = {
                if (fn.symbol.owner.isRefinementClass && fn.symbol.allOverriddenSymbols.isEmpty)
                  ApplyDynamic(qual, args) setSymbol fn.symbol setPos tree.pos
                else tree
              }
              fn match {
                case Select(qual, _) => doDynamic(fn, qual)
                case TypeApply(fni@Select(qual, _), _) => doDynamic(fni, qual)// type parameters are irrelevant in case of dynamic call
                case _ =>
                  tree
              }
            }

          case Select(_, _) =>
            if (tree.symbol.owner.isRefinementClass) {
              val overridden = tree.symbol.allOverriddenSymbols
              assert(!overridden.isEmpty, tree.symbol)
              tree.symbol = overridden.head
            }
            tree

          case Template(parents, self, body) =>
            assert(!currentOwner.isImplClass)
            //Console.println("checking no dble defs " + tree)//DEBUG
            checkNoDoubleDefs(tree.symbol.owner)
            treeCopy.Template(tree, parents, emptyValDef, addBridges(body, currentOwner))

          case Match(selector, cases) =>
            Match(Typed(selector, TypeTree(selector.tpe)), cases)

          case Literal(ct) if ct.tag == ClassTag
                           && ct.typeValue.typeSymbol != definitions.UnitClass =>
            treeCopy.Literal(tree, Constant(erasure(ct.typeValue)))

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
            case class MyError(count : Int, ex : AssertionError) extends Error(ex.getMessage)
            try {
              super.transform(tree1) setType null
            } catch {
              case e @ MyError(n, ex) if n > 5 =>  throw e
              case MyError(n,ex) =>
                Console.println(tree1)
                throw MyError(n + 1, ex)
//              case ex : AssertionError =>
//                Console.println(tree1)
//                throw MyError(0, ex)
//              case ex => throw ex
            }
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
