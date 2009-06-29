/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

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

  case class TypeComparison(x: Type, y: Type) {
    def xIsaY = x <:< y
    def yIsaX = y <:< x
    def xEqY  = y =:= x
    def xIgnY = !xIsaY && !yIsaX && !xEqY
    def eqSymbol = cmpSymbols(x, y)
    def eqPrefix = x.prefix =:= y.prefix

    private def cmpSymbols(t1: Type, t2: Type) = t1.typeSymbol eq t2.typeSymbol
    // true if t2 is a parent of t1.  Should this be rather: tpe.baseTypes.exists...?
    private def  parenthood(t1: Type, t2: Type) = t1.parents.exists(p => cmpSymbols(p, t2))
    // true if t1 is direct subtype of t2 (can't use just <:< cause have to take variance into account)
    private def subtypehood(t1: Type, t2: Type) = t1.parents.exists(p => cmpSymbols(p, t2) && p <:< t2)

    def yParentsX = parenthood(x, y)
    def xParentsY = parenthood(y, x)
    def xExtendsY = subtypehood(x, y)
    def yExtendsX = subtypehood(y, x)

    object erased {
      import Types._
      /** an approximation of _tp1 <:< tp2 that ignores _ types. this code is wrong,
       *  ideally there is a better way to do it, and ideally defined in Types.scala
       */
      private def cmpErased(t1: Type, t2: Type) = (t1, t2) match {
        case (_: TypeRef, _: TypeRef) => (eqPrefix && eqSymbol && !t1.isArray) || parenthood(t1, t2)
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
        case TypeRef(_, sym, arg::Nil) if sym eq EqualsPatternClass => arg
        case _                                                      => t
      }
      lazy val tpe = tpeWRTEquality(_tpe)

      // The several different logics in these tests are intentionally grouped here to
      // draw attention to them.  To the extent that the differences are intentional,
      // the thinking behind the distinctions needs to be documented.
      def       isInt = tpe =:= IntClass.tpe
      def      isChar = tpe =:= CharClass.tpe
      def    isAnyRef = tpe <:< AnyRefClass.tpe
      def   isBoolean = tpe.typeSymbol eq BooleanClass
      def     isArray = tpe.typeSymbol eq ArrayClass
      def   isNothing = tpe.typeSymbol eq NothingClass
      def isCaseClass = tpe.typeSymbol hasFlag Flags.CASE

      def cmp(other: Type): TypeComparison = TypeComparison(tpe, tpeWRTEquality(other))

      def coversSym(sym: Symbol) = {
        lazy val lmoc = sym.linkedModuleOfClass
        val symtpe =
          if ((sym hasFlag Flags.MODULE) && (lmoc ne NoSymbol))
            singleType(sym.tpe.prefix, lmoc)   // e.g. None, Nil
          else sym.tpe

        (tpe.typeSymbol == sym) ||
        (symtpe <:< tpe) ||
        (symtpe.parents.exists(_.typeSymbol eq tpe.typeSymbol)) || // e.g. Some[Int] <: Option[&b]
        (tpe.prefix.memberType(sym) <:< tpe)  // outer, see combinator.lexical.Scanner
      }
    }

    // used as argument to `EqualsPatternClass'
    case class PseudoType(o: Tree) extends SimpleTypeProxy {
      override def underlying: Type = o.tpe
      override def safeToString: String = "PseudoType("+o+")"
    }
  }

  final def DBG(x: => String)   = if (settings.debug.value) Console.println(x)

  final def getDummies(i: Int): List[Tree] = List.make(i, EmptyTree)

  def makeBind(vs:List[Symbol], pat:Tree): Tree =
    if (vs eq Nil) pat else Bind(vs.head, makeBind(vs.tail, pat)) setType pat.tpe

  def mkTypedBind(vs: List[Symbol], tpe: Type) =
    makeBind(vs, Typed(WILD(tpe), TypeTree(tpe)) setType tpe)

  def mkEmptyTreeBind(vs: List[Symbol], tpe: Type) =
    makeBind(vs, Typed(EmptyTree, TypeTree(tpe)) setType tpe)

  def mkEqualsRef(xs: List[Type]) = typeRef(NoPrefix, definitions.EqualsPatternClass, xs)

  def normalizedListPattern(pats:List[Tree], tptArg:Type): Tree = pats match {
    case Nil                            => gen.mkNil
    case (sp @ Strip(_, _: Star)) :: _  => makeBind(definedVars(sp), WILD(sp.tpe))
    case x :: xs                        =>
      var resType: Type = null;
      val consType: Type = definitions.ConsClass.primaryConstructor.tpe match {
        case mt @ MethodType(args, res @ TypeRef(pre,sym,origArgs)) =>
          val listType = TypeRef(pre, definitions.ListClass, List(tptArg))
               resType = TypeRef(pre, sym                  , List(tptArg))

          val dummyMethod = new TermSymbol(NoSymbol, NoPosition, "matching$dummy")
          MethodType(dummyMethod.newSyntheticValueParams(List(tptArg, listType)), resType)
      }
      Apply(TypeTree(consType), List(x, normalizedListPattern(xs, tptArg))) setType resType
  }

  // if Apply target !isType and takes no args, returns target prefix and symbol
  object Apply_Value {
    def unapply(x: Any) = x match {
      case x @ Apply(fun, args) if !fun.isType && args.isEmpty  => Some(x.tpe.prefix, x.symbol)
      case _                                                    => None
    }
  }

  // if Apply tpe !isCaseClass and Apply_Value says false, return the Apply target
  object Apply_Function {
    /* see t301 */
    def isApplyFunction(o: Apply) = !o.tpe.isCaseClass || !Apply_Value.unapply(o).isEmpty
    def unapply(x: Any) = x match {
      case x @ Apply(fun, Nil) if isApplyFunction(x)  => Some(fun)
      case _                                          => None
    }
  }

  // if Apply target isType (? what does this imply exactly) returns type and arguments.
  object Apply_CaseClass {
    def unapply(x: Any) = x match {
      case x @ Apply(fun, args) if fun.isType => Some(x.tpe, args)
      case _                                  => None
    }
  }

  // TODO - this doesn't work! For some reason this sometimes returns false when encapsulated
  // in an object like this, when it returns true if the same logic is applied literally in
  // the classifyPat match in ParallelMatching.  I couldn't figure out why, and it concerns
  // me - if this isn't working maybe other unapply objects aren't working right either.  The big
  // problem with using unapply and type matching is that if something's amiss, it will simply fail
  // silently, with the bug most likely manifesting at some unknown later point.
  // DRM: This problem is almost certainly an instance of bug 1697
  object Ident_Or_Empty {
    def unapply(x: Any) = x match {
      case Ident(nme.WILDCARD) | EmptyTree | _: Typed | _: Literal => true
      // this returns false, and then a line identical the one above will match
      // back in PM.scala.
      case _ => false
    }
  }

  object UnApply_TypeApply {
    def unapply(x: UnApply) = x match {
      case UnApply(Apply(TypeApply(sel @ Select(stor, nme.unapplySeq), List(tptArg)), _), ArrayValue(_, xs)::Nil)
        if (stor.symbol eq definitions.ListModule)  => Some(tptArg, xs)
      case _                                        => None
    }
  }

  object __UnApply {
    private def paramType(fn: Tree) = fn.tpe match { case m: MethodType => m.paramTypes.head }
    def unapply(x: Tree) = x match {
      case Strip(vs, UnApply(Apply(fn, _), args)) => Some(vs, paramType(fn), args)
      case _                                      => None
    }
  }

  /** returns if pattern can be considered a no-op test ??for expected type?? */
  final def isDefaultPattern(pattern: Tree): Boolean = pattern match {
    case Bind(_, p)            => isDefaultPattern(p)
    case EmptyTree             => true // dummy
    case Ident(nme.WILDCARD)   => true
    case _                     => false
// -- what about the following? still have to test "ne null" :/
//  case Typed(nme.WILDCARD,_) => pattern.tpe <:< scrut.tpe
  }

  /** returns all variables that are binding the given pattern
   *  @param   x a pattern
   *  @return  vs variables bound, p pattern proper
   */
  final def strip(x: Tree): (Set[Symbol], Tree) = x match {
    case b @ Bind(_,pat) => val (vs, p) = strip(pat); (vs + b.symbol, p)
    case _               => (emptySymbolSet, x)
  }

  object Strip  { def unapply(x: Tree): Option[(Set[Symbol], Tree)] = Some(strip(x))  }

  object BoundVariables {
    def unapply(x: Tree): Option[List[Symbol]] = Some(Pattern(x).boundVariables)
  }
  object Stripped {
    def unapply(x: Tree): Option[Tree] = Some(Pattern(x).stripped)
  }

  final def definedVars(x: Tree): List[Symbol] = {
    implicit def listToStream[T](xs: List[T]): Stream[T] = xs.toStream
    def definedVars1(x: Tree): Stream[Symbol] = x match {
      case Apply(_, args)     => definedVars2(args)
      case b @ Bind(_,p)      => Stream.cons(b.symbol, definedVars1(p))
      case Typed(p,_)         => definedVars1(p)    // otherwise x @ (_:T)
      case UnApply(_,args)    => definedVars2(args)
      case ArrayValue(_,xs)   => definedVars2(xs)
      case _                  => Nil
    }
    def definedVars2(args: Stream[Tree]): Stream[Symbol] = args flatMap definedVars1

    definedVars1(x).reverse.toList
  }

  /** pvar: the symbol of the pattern variable
   *  temp: the temp variable that holds the actual value
   */
  case class Binding(pvar: Symbol, temp: Symbol)

  case class Bindings(bindings: Binding*) extends Function1[Symbol, Option[Ident]] {
    def add(vs: Iterable[Symbol], temp: Symbol): Bindings =
      Bindings(vs.toList.map(Binding(_, temp)) ++ bindings : _*)

    def apply(v: Symbol): Option[Ident] = bindings.find(_.pvar eq v) match {
      case Some(b)  => Some(Ident(b.temp) setType v.tpe)
      case None     => None // abort("Symbol " + v + " has no binding in " + bindings)
    }

    /**
     * The corresponding list of value definitions.
     */
    final def targetParams(implicit typer: Typer): List[ValDef] =
      for (Binding(v, t) <- bindings.toList) yield
        VAL(v) === (typer typed (if (t.tpe <:< v.tpe) ID(t) else (ID(t) AS_ANY v.tpe)))
  }

  val NoBinding: Bindings = Bindings()
}
