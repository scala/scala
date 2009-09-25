/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc
package matching

import scala.tools.nsc.util.{Position, NoPosition}

/**
 *  @author Burak Emir
 */
trait PatternNodes extends ast.TreeDSL
{
  self: transform.ExplicitOuter =>

  import global.{ typer => _, _ }
  import analyzer.Typer
  import symtab.Flags
  import Types._
  import CODE._
  import definitions.{ ConsClass, ListClass, AnyRefClass, EqualsPatternClass, ListModule }

  type TypeComparison = (Type, Type) => Boolean

  // Tests on Types
  def isEquals(t: Type)       = cond(t) { case TypeRef(_, EqualsPatternClass, _) => true }
  def isAnyRef(t: Type)       = t <:< AnyRefClass.tpe
  def isCaseClass(t: Type)    = t.typeSymbol hasFlag Flags.CASE

  // Comparisons on types
  // def sameSymbols: TypeComparison  = _.typeSymbol eq _.typeSymbol
  // def samePrefix: TypeComparison   = _.prefix =:= _.prefix
  // def isSubErased: TypeComparison  = (t1, t2) => cond((t1, t2)) {
  //   case (_: TypeRef, _: TypeRef) => !t1.isArray && samePrefix(t1,t2) && (sameSymbols(t1,t2) || isSubClass(t1, t2))
  // }

  // def isSubClass: TypeComparison   = (t1, t2) => t1.baseClasses exists (_ eq t2.typeSymbol)
  // def isSubType: TypeComparison    = (t1, t2) => isSubClass(t1, t2) && (t1 <:< t2)
  // def isPatMatch: TypeComparison   = (t1, t2) => isSubType(t1, t2)

  def decodedEqualsType(tpe: Type) =
    condOpt(tpe) { case TypeRef(_, EqualsPatternClass, List(arg))  => arg } getOrElse (tpe)

  // If we write isSubtypeOf like:
  //
  //   = t1.baseTypeSeq exists (_ =:= t2)
  //
  // ..then all tests pass except for test/files/run/bug1434.scala, which involves:
  // class A[T], B extends A[Any], C extends B ; ... match { case _: A[_] => .. ; case _: C => .. ; case _: B => .. }
  // and a match error is thrown, which is interesting because either of C or B should match.
  def isSubtypeOf(t1: Type, t2: Type) = t1.baseTypeSeq exists (p => cmpSymbols(p, t2))
  def cmpSymbols(t1: Type, t2: Type)  = t1.typeSymbol eq t2.typeSymbol

  case class TypeComp(x: Type, y: Type) {
    def xIsaY = x <:< y
    def yIsaX = y <:< x
    def eqSymbol = cmpSymbols(x, y)
    def eqPrefix = x.prefix =:= y.prefix

    object erased {
      import Types._
      /** an approximation of _tp1 <:< tp2 that ignores _ types. this code is wrong,
       *  ideally there is a better way to do it, and ideally defined in Types.scala
       */
      private def cmpErased(t1: Type, t2: Type) = (t1, t2) match {
        case (_: TypeRef, _: TypeRef) => !t1.isArray && eqPrefix && (eqSymbol || isSubtypeOf(t1, t2))
        case _ => false
      }

      def xIsaY = cmpErased(x, y)
      def yIsaX = cmpErased(y, x)
    }
  }

  object Types {
    import definitions._
    implicit def enrichType(_tpe: Type): RichType = new RichType(_tpe)

    class RichType(_tpe: Type) {
      /* equality checks for named constant patterns like "Foo()" are encoded as "_:<equals>[Foo().type]"
       * and later compiled to "if(Foo() == scrutinee) ...". This method extracts type information from
       * such an encoded type, which is used in optimization. If the argument is not an encoded equals
       * test, it is returned as is.
       */
      private def tpeWRTEquality(t: Type) = t match {
        case TypeRef(_, EqualsPatternClass, List(arg))  => arg
        case _                                          => t
      }
      lazy val tpe = tpeWRTEquality(_tpe)

      // These tests for final classes can inspect the typeSymbol
      private def is(s: Symbol) = tpe.typeSymbol eq s
      def      isByte = is(ByteClass)
      def     isShort = is(ShortClass)
      def       isInt = is(IntClass)
      def      isChar = is(CharClass)
      def   isBoolean = is(BooleanClass)
      def   isNothing = is(NothingClass)
      def     isArray = is(ArrayClass)

      def cmp(other: Type): TypeComp = TypeComp(tpe, tpeWRTEquality(other))

      def coversSym(sym: Symbol) = {
        lazy val lmoc = sym.linkedModuleOfClass
        val symtpe =
          if ((sym hasFlag Flags.MODULE) && (lmoc ne NoSymbol))
            singleType(sym.tpe.prefix, lmoc)   // e.g. None, Nil
          else sym.tpe

        (tpe.typeSymbol == sym) ||
        (symtpe <:< tpe) ||
        (symtpe.parents exists (x => cmpSymbols(x, tpe))) || // e.g. Some[Int] <: Option[&b]
        ((tpe.prefix memberType sym) <:< tpe)  // outer, see combinator.lexical.Scanner
      }
    }

    // used as argument to `EqualsPatternClass'
    case class PseudoType(o: Tree) extends SimpleTypeProxy {
      override def underlying: Type = o.tpe
      override def safeToString: String = "PseudoType("+o+")"
    }
  }

  final def getDummies(i: Int): List[Tree] = List.fill(i)(EmptyTree)

  def makeBind(vs: List[Symbol], pat: Tree): Tree = vs match {
    case Nil      => pat
    case x :: xs  => Bind(x, makeBind(xs, pat)) setType pat.tpe
  }

  private def mkBind(vs: List[Symbol], tpe: Type, arg: Tree) =
    makeBind(vs, Typed(arg, TypeTree(tpe)) setType tpe)

  def mkTypedBind(vs: List[Symbol], tpe: Type)      = mkBind(vs, tpe, WILD(tpe))
  def mkEmptyTreeBind(vs: List[Symbol], tpe: Type)  = mkBind(vs, tpe, EmptyTree)
  def mkEqualsRef(xs: List[Type])                   = typeRef(NoPrefix, EqualsPatternClass, xs)

  /** For folding a list into a well-typed x :: y :: etc :: tree. */
  private def listFolder(tpe: Type) = {
    val MethodType(_, TypeRef(pre, sym, _)) = ConsClass.primaryConstructor.tpe
    val consRef                             = typeRef(pre, sym, List(tpe))
    val listRef                             = typeRef(pre, ListClass, List(tpe))

    def fold(x: Tree, xs: Tree) = x match {
      case sp @ Strip(_, _: Star) => makeBind(definedVars(sp), WILD(sp.tpe))
      case _                      =>
        val dummyMethod = new TermSymbol(NoSymbol, NoPosition, "matching$dummy")
        val consType    = MethodType(dummyMethod newSyntheticValueParams List(tpe, listRef), consRef)

        Apply(TypeTree(consType), List(x, xs)) setType consRef
    }

    fold _
  }

  def normalizedListPattern(pats: List[Tree], tptArg: Type): Tree =
    pats.foldRight(gen.mkNil)(listFolder(tptArg))

  // An Apply that's a constructor pattern (case class)
  // foo match { case C() => true }
  object Apply_CaseClass {
    def unapply(x: Any) = condOpt(x) {
      case x @ Apply(fn, args) if fn.isType => (x.tpe, args)
    }
  }

  // No-args Apply where fn is not a type - looks like, case object with prefix?
  //
  // class Pip {
  //   object opcodes { case object EmptyInstr }
  //   def bop(x: Any) = x match { case opcodes.EmptyInstr => true }
  // }
  object Apply_Value {
    def unapply(x: Any) = condOpt(x) {
      case x @ Apply(fn, Nil) if !fn.isType => (x.tpe.prefix, x.symbol)
    }
  }

  // No-args Apply for all the other cases
  // val Bop = Nil
  // def foo(x: Any) = x match { case Bop => true }
  object Apply_Function {
    def isApplyFunction(t: Apply) = cond(t) {
      case Apply_Value(_, _)        => true
      case x if !isCaseClass(x.tpe) => true
    }
    def unapply(x: Any) = condOpt(x) {
      case x @ Apply(fn, Nil) if isApplyFunction(x) => fn
    }
  }

  // unapplySeq extractor
  // val List(x,y) = List(1,2)
  object UnapplySeq {
    private object TypeApp {
      def unapply(x: Any) = condOpt(x) {
        case TypeApply(sel @ Select(stor, nme.unapplySeq), List(tpe)) if stor.symbol eq ListModule => tpe
      }
    }
    def unapply(x: UnApply) = condOpt(x) {
      case UnApply(Apply(TypeApp(tptArg), _), List(ArrayValue(_, xs))) => (tptArg, xs)
    }
  }

  // unapply extractor
  // val Pair(_,x) = Pair(1,2)
  object __UnApply {
    private def paramType(fn: Tree) = fn.tpe match { case m: MethodType => m.paramTypes.head }
    def unapply(x: Tree) = condOpt(x) {
      case Strip(vs, UnApply(Apply(fn, _), args)) => (vs, paramType(fn), args)
    }
  }

  // break a pattern down into bound variables and underlying tree.
  object Strip {
    private def strip(t: Tree, syms: List[Symbol] = Nil): (Tree, List[Symbol]) = t match {
      case b @ Bind(_, pat) => strip(pat, b.symbol :: syms)
      case _                => (t, syms)
    }
    def unapply(x: Tree): Option[(List[Symbol], Tree)] = Some(strip(x).swap)
  }

  object Stripped {
    def unapply(x: Tree): Option[Tree] = Some(unbind(x))
  }

  final def definedVars(x: Tree): List[Symbol] = {
    def vars(x: Tree): List[Symbol] = x match {
      case Apply(_, args)     => args flatMap vars
      case b @ Bind(_, p)     => b.symbol :: vars(p)
      case Typed(p, _)        => vars(p)              // otherwise x @ (_:T)
      case UnApply(_, args)   => args flatMap vars
      case ArrayValue(_, xs)  => xs flatMap vars
      case x                  => Nil
    }
    vars(x) reverse
  }

  /** pvar: the symbol of the pattern variable
   *  tvar: the temporary variable that holds the actual value
   */
  case class Binding(pvar: Symbol, tvar: Symbol) {
    override def toString() = "%s: %s @ %s: %s".format(pvar.name, pvar.tpe, tvar.name, tvar.tpe)
  }

  case class Bindings(bindings: Binding*) extends Function1[Symbol, Option[Ident]] {
    private def castIfNeeded(pvar: Symbol, tvar: Symbol) =
      if (tvar.tpe <:< pvar.tpe) ID(tvar)
      else ID(tvar) AS_ANY pvar.tpe

    def add(vs: Iterable[Symbol], tvar: Symbol): Bindings = {
      def newBinding(v: Symbol) = {
        // see bug #1843 for the consequences of not setting info.
        // there is surely a better way to do this, especially since
        // this looks to be the only usage of containsTp anywhere
        // in the compiler, but it suffices for now.
        if (tvar.info containsTp WildcardType)
          tvar setInfo v.info

        Binding(v, tvar)
      }
      val newBindings = vs.toList map newBinding
      Bindings(newBindings ++ bindings: _*)
    }

    def apply(v: Symbol): Option[Ident] =
      bindings find (_.pvar eq v) map (x => Ident(x.tvar) setType v.tpe)

    override def toString() =
      if (bindings.isEmpty) "" else bindings.mkString(" Bound(", ", ", ")")

    /** The corresponding list of value definitions. */
    final def targetParams(implicit typer: Typer): List[ValDef] =
      for (Binding(v, t) <- bindings.toList) yield
        VAL(v) === (typer typed castIfNeeded(v, t))
  }

  val NoBinding: Bindings = Bindings()
}
