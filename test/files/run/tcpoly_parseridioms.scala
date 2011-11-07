trait Parsers {
  type Input = List[Char]
  
  sealed class ParseResult[+t](val next: Input)
    case class Success[+t](override val next: Input, result: t) extends ParseResult[t](next)  
    case class Failure(override val next: Input, msg: String) extends ParseResult[Nothing](next)
  
  abstract class Parser[+t] {
    def apply(in: Input): ParseResult[t]
  }

  // sequence
  def sq[T, U](a: => Parser[T], b: => Parser[U]): Parser[Pair[T, U]] =  new Parser[Pair[T, U]] {
      def apply(in: Input): ParseResult[Pair[T, U]] = a(in) match {
        case Success(next, x) => b(next) match {
          case Success(next2, y) => Success(next2, Pair(x,y))
          case Failure(_, msg) => Failure(in, msg)
        }
        case Failure(_, msg) => Failure(in, msg)  
      }
    }

  // ordered choice
  def or[T, U <: T](a: => Parser[T], b: => Parser[U]): Parser[T] =  new Parser[T] {
    def apply(in: Input): ParseResult[T] = a(in) match {
      case Success(next, x) => Success(next, x)
      case Failure(_, _) => b(in) match {
        case Success(next, y) => Success(next, y)
        case Failure(_, msg) => Failure(in, msg)
      }
    }
  }

  // lifting
  def lift[T, U](f: T => U)(a: => Parser[T]): Parser[U] = new Parser[U] {
    def apply(in: Input): ParseResult[U] = a(in) match {
      case Success(n, x) => Success(n, f(x))
      case Failure(n, msg) => Failure(n, msg)
    }
  }
  
  def accept[T](c: Char, r: T): Parser[T] = new Parser[T] {
    def apply(in: Input) = in match {
      case c2 :: n if c2 == c => Success(n, r)
      case n => Failure(n, "expected "+c+" at the head of "+n)
    }
  }
  
  def apply_++[s, tt](fun: Parser[s => tt], arg: Parser[s]): Parser[tt] = lift[Pair[s=>tt, s], tt]({case Pair(f, a) => f(a)})(sq(fun, arg)) 
    
  def success[u](v: u): Parser[u] = new Parser[u] {
    def apply(in: Input) = Success(in, v)
  }
    
}

trait Idioms {
  trait Idiom[idi[x]] {
    def liftedApply[s, t](fun: idi[s => t])(arg: idi[s]): idi[t]
    def pure[a](x: a): idi[a]
    def pureMethod[a](name: String, x: a): idi[a] = pure(x) // hack for Mirrors: allow passing of method names
  }

  class IdiomaticTarget[idi[x], idiom <: Idiom[idi], s](i: idiom, tgt: s) { 
    def dot [t](fun: s => t, name: String) = new IdiomaticApp2[idi, idiom, t](i, i.liftedApply(i.pureMethod(name, fun))(i.pure(tgt)))
  } // TODO: `.` -->  java.lang.ClassFormatError: Illegal method name "." in class Idioms$Id$

  class IdiomaticFunction[idi[x], idiom <: Idiom[idi], s, t](i: idiom, fun: s => t) { 
    def <| (a: idi[s]) = new IdiomaticApp[idi, idiom, t](i, i.liftedApply(i.pure(fun))(a))
  }

  class IdiomaticApp[idi[x], idiom <: Idiom[idi], x](i: idiom, a: idi[x]) {
    // where x <: s=>t -- TODO can this be expressed without generalised constraints?
    def <> [s, t](b: idi[s]) = new IdiomaticApp[idi, idiom, t](i, i.liftedApply(a.asInstanceOf[idi[s=>t]])(b))
    
    def |> : idi[x] = a
  }
  
  class IdiomaticApp2[idi[x], idiom <: Idiom[idi], x](i: idiom, a: idi[x]) extends IdiomaticApp[idi, idiom, x](i, a) {
    def <| [s, t](b: idi[s]) = <>[s,t](b)
  }
}

trait ParserIdioms extends Parsers with Idioms {
  object ParserIdiom extends Idiom[Parser] {
    def liftedApply[s, t](fun: Parser[s => t])(arg: Parser[s]): Parser[t] = apply_++(fun, arg)
    def pure[a](x: a): Parser[a] = success(x)
  }
  
  implicit def parserIdiomFun[s, t](fun: s=>t): IdiomaticFunction[Parser, ParserIdiom.type, s, t] = 
    new IdiomaticFunction[Parser, ParserIdiom.type, s, t](ParserIdiom, fun)
  implicit def parserIdiomTgt[s](tgt: s): IdiomaticTarget[Parser, ParserIdiom.type, s] = 
    new IdiomaticTarget[Parser, ParserIdiom.type, s](ParserIdiom, tgt)
    
  trait Expr
  case class Plus(a: Int, b: Int) extends Expr
  
  def num = or(accept('0', 0), or(accept('1', 1),accept('2', 2)))
  
  // TODO: how can parserIdiom(curry2(_)) be omitted? 
  def expr: Parser[Expr] = parserIdiomFun(curry2(Plus)) <| num <> num |>
   
  implicit def curry2[s,t,u](fun: (s, t)=>u)(a: s)(b: t) = fun(a, b)
  implicit def curry3[r,s,t,u](fun: (r,s, t)=>u)(a: r)(b: s)(c: t) = fun(a, b, c)  
}

object Test extends ParserIdioms with App {
  println(expr("12".toList))
}
