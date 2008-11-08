/* NSC -- new Scala compiler
 * Copyright 2005-2007 LAMP/EPFL
 * @author Burak Emir
 */
// $Id$

package scala.tools.nsc.matching

import scala.tools.nsc.util.{Position, NoPosition}

/**
 *  @author Burak Emir
 */
trait PatternNodes { self: transform.ExplicitOuter =>
  import global._
  import symtab.Flags
  final def DBG(x: => String)   = if (settings.debug.value) Console.println(x)

  final def getDummies(i: Int): List[Tree] = List.make(i, EmptyTree)

  def makeBind(vs:List[Symbol], pat:Tree): Tree =
    if (vs eq Nil) pat else Bind(vs.head, makeBind(vs.tail, pat)) setType pat.tpe

  def normalizedListPattern(pats:List[Tree], tptArg:Type): Tree = pats match {
    case Nil   => gen.mkAttributedRef(definitions.NilModule)
    case (sp @ Strip(_, _: Star)) :: xs => makeBind(definedVars(sp), Ident(nme.WILDCARD) setType sp.tpe)
    // case sp::xs if strip2(sp).isInstanceOf[Star] =>
    //   makeBind(definedVars(sp), Ident(nme.WILDCARD) setType sp.tpe)
    case x::xs =>
      var resType: Type = null;
      val consType: Type = definitions.ConsClass.primaryConstructor.tpe match {
        case mt @ MethodType(args, res @ TypeRef(pre,sym,origArgs)) =>
          val listType = TypeRef(pre, definitions.ListClass, List(tptArg))
               resType = TypeRef(pre, sym                  , List(tptArg))

          MethodType(List(tptArg, listType), resType)
      }
      Apply(TypeTree(consType),List(x,normalizedListPattern(xs,tptArg))).setType(resType)
  }

  object Apply_Value {
    def unapply(x:Apply) = if (!x.fun.isType && x.args.isEmpty) Some(x.tpe.prefix, x.symbol) else None
  }

  object Apply_CaseClass_NoArgs {
    def unapply(x:Apply) = if (x.fun.isType && x.args.isEmpty) Some(x.tpe) else None
  }
  object Apply_CaseClass_WithArgs {
    def unapply(x:Apply) = x.fun.isType
  }

  object __UnApply {
    def unapply(x:Tree) = strip(x) match {
      case (vs, UnApply(Apply(fn, _), args)) =>
        val argtpe = fn.tpe.asInstanceOf[MethodType].paramTypes.head
        Some(Tuple3(vs,argtpe,args))
      case _                      => None
    }
  }

  /* equality checks for named constant patterns like "Foo()" are encoded as "_:<equals>[Foo().type]"
   * and later compiled to "if(Foo() == scrutinee) ...". This method extracts type information from
   * such an encoded type, which is used in optimization. If the argument is not an encoded equals
   * test, it is returned as is.
   */
  def patternType_wrtEquals(pattpe:Type) = pattpe match {
    case TypeRef(_,sym,arg::Nil) if sym eq definitions.EqualsPatternClass =>
      arg
    case x => x
  }

  /** returns if pattern can be considered a no-op test ??for expected type?? */
  final def isDefaultPattern(pattern: Tree): Boolean = pattern match {
    case Bind(_, p)            => isDefaultPattern(p)
    case EmptyTree             => true // dummy
    case Ident(nme.WILDCARD)   => true
    case _                     => false
// -- what about the following? still have to test "ne null" :/
//  case Typed(nme.WILDCARD,_) => pattern.tpe <:< scrutinee.tpe
  }

  /** returns all variables that are binding the given pattern
   *  @param   x a pattern
   *  @return  vs variables bound, p pattern proper
   */
  final def strip(x: Tree): (Set[Symbol], Tree) = x match {
    case b @ Bind(_,pat) => val (vs, p) = strip(pat); (vs + b.symbol, p)
    case z               => (emptySymbolSet, z)
  }

  final def strip1(x: Tree): Set[Symbol] = strip(x)._1
  final def strip2(x: Tree): Tree = strip(x)._2;

  object Strip {
    def unapply(x: Tree): Option[(Set[Symbol], Tree)] = Some(strip(x))
  }

  final def isCaseClass(tpe: Type): Boolean =
    tpe.typeSymbol hasFlag Flags.CASE

  final def isEqualsPattern(tpe: Type): Boolean = tpe match {
    case TypeRef(_, sym, _) => sym eq definitions.EqualsPatternClass
    case _                  => false
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
   *  next: next binding
   */
  case class Binding(pvar:Symbol, temp:Symbol, private val next: Binding) extends Function1[Symbol, Ident]{
    def add(vs : Iterable[Symbol], temp : Symbol): Binding =
      vs.foldLeft(this)((x, y) => Binding(y, temp, x))

    /** this is just to produce debug output, ListBuffer needs an equals method?! */
    override def equals(x:Any) = {
      x match {
        case NoBinding               => false
        case Binding(pv2,tmp2,next2) => (pvar eq pv2) && (temp eq tmp2) && (next==next2)
      }
    }
    def apply(v:Symbol): Ident = {
      if (v eq pvar) Ident(temp).setType(v.tpe) else next(v)
    }
  }

  object NoBinding extends Binding(null, null, null) {
    override def apply(v:Symbol) = null // not found, means bound elsewhere (x @ unapply-call)
    override def toString = "."
    override def equals(x:Any) = x.isInstanceOf[Binding] && (x.asInstanceOf[Binding] eq this)
  }
}
