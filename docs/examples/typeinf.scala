package examples

object typeinf {

trait Term {}

case class Var(x: String)                   extends Term {
  override def toString() = x
}
case class Lam(x: String, e: Term)          extends Term {
  override def toString() = "(\\" + x + "." + e + ")"
}
case class App(f: Term, e: Term)            extends Term {
  override def toString() = "(" + f + " " + e + ")"
}
case class Let(x: String, e: Term, f: Term) extends Term {
  override def toString() = "let " + x + " = " + e + " in " + f
}

sealed trait Type {}
case class Tyvar(a: String) extends Type {
  override def toString() = a
}
case class Arrow(t1: Type, t2: Type) extends Type {
  override def toString() = "(" + t1 + "->" + t2 + ")"
}
case class Tycon(k: String, ts: List[Type]) extends Type {
  override def toString() =
    k + (if (ts.isEmpty) "" else ts.mkString("[", ",", "]"))
}

object typeInfer {

  private var n: Int = 0
  def newTyvar(): Type = { n += 1; Tyvar("a" + n) }

  trait Subst extends Function1[Type, Type] {
    def lookup(x: Tyvar): Type
    def apply(t: Type): Type = t match {
      case tv @ Tyvar(a) => val u = lookup(tv); if (t == u) t else apply(u)
      case Arrow(t1, t2) => Arrow(apply(t1), apply(t2))
      case Tycon(k, ts) => Tycon(k, ts map apply)
    }
    def extend(x: Tyvar, t: Type) = new Subst {
      def lookup(y: Tyvar): Type = if (x == y) t else Subst.this.lookup(y)
    }
  }

  val emptySubst = new Subst { def lookup(t: Tyvar): Type = t }

  case class TypeScheme(tyvars: List[Tyvar], tpe: Type) {
    def newInstance: Type =
      (emptySubst /: tyvars) ((s, tv) => s.extend(tv, newTyvar())) (tpe)
  }

  type Env = List[Pair[String, TypeScheme]]

  def lookup(env: Env, x: String): TypeScheme = env match {
    case List() => null
    case Pair(y, t) :: env1 => if (x == y) t else lookup(env1, x)
  }

  def gen(env: Env, t: Type): TypeScheme =
    TypeScheme(tyvars(t) diff tyvars(env), t)

  def tyvars(t: Type): List[Tyvar] = t match {
    case tv @ Tyvar(a) => List(tv)
    case Arrow(t1, t2) => tyvars(t1) union tyvars(t2)
    case Tycon(k, ts) => (List[Tyvar]() /: ts) ((tvs, t) => tvs union tyvars(t))
  }

  def tyvars(ts: TypeScheme): List[Tyvar] = 
    tyvars(ts.tpe) diff ts.tyvars;

  def tyvars(env: Env): List[Tyvar] =
    (List[Tyvar]() /: env) ((tvs, nt) => tvs union tyvars(nt._2))

  def mgu(t: Type, u: Type, s: Subst): Subst = Pair(s(t), s(u)) match {
    case Pair(Tyvar(a), Tyvar(b)) if (a == b) => 
      s
    case Pair(Tyvar(a), _) if !(tyvars(u) contains a) =>
      s.extend(Tyvar(a), u)
    case Pair(_, Tyvar(a)) =>
      mgu(u, t, s)
    case Pair(Arrow(t1, t2), Arrow(u1, u2)) =>
      mgu(t1, u1, mgu(t2, u2, s))
    case Pair(Tycon(k1, ts), Tycon(k2, us)) if (k1 == k2) =>
      (s /: (ts zip us)) ((s, tu) => mgu(tu._1, tu._2, s))
    case _ =>
      throw new TypeError("cannot unify " + s(t) + " with " + s(u))
  }

  case class TypeError(s: String) extends Exception(s) {}

  def tp(env: Env, e: Term, t: Type, s: Subst): Subst = {
    current = e
    e match {
      case Var(x) =>
        val u = lookup(env, x)
        if (u == null) throw new TypeError("undefined: " + x)
        else mgu(u.newInstance, t, s)

      case Lam(x, e1) =>
        val a, b = newTyvar()
        val s1 = mgu(t, Arrow(a, b), s)
        val env1 = Pair(x, TypeScheme(List(), a)) :: env
        tp(env1, e1, b, s1)

      case App(e1, e2) =>
        val a = newTyvar()
        val s1 = tp(env, e1, Arrow(a, t), s)
        tp(env, e2, a, s1)

      case Let(x, e1, e2) =>
        val a = newTyvar()
        val s1 = tp(env, e1, a, s)
        tp(Pair(x, gen(env, s1(a))) :: env, e2, t, s1)
    }
  }
  var current: Term = null

  def typeOf(env: Env, e: Term): Type = {
    val a = newTyvar()
    tp(env, e, a, emptySubst)(a)
  }
}

  object predefined {
    val booleanType = Tycon("Boolean", List())
    val intType = Tycon("Int", List())
    def listType(t: Type) = Tycon("List", List(t))

    private def gen(t: Type): typeInfer.TypeScheme = typeInfer.gen(List(), t)
    private val a = typeInfer.newTyvar()
    val env = List(
/*
      Pair("true", gen(booleanType)),
      Pair("false", gen(booleanType)),
      Pair("if", gen(Arrow(booleanType, Arrow(a, Arrow(a, a))))),
      Pair("zero", gen(intType)),
      Pair("succ", gen(Arrow(intType, intType))),
      Pair("nil", gen(listType(a))),
      Pair("cons", gen(Arrow(a, Arrow(listType(a), listType(a))))),
      Pair("isEmpty", gen(Arrow(listType(a), booleanType))),
      Pair("head", gen(Arrow(listType(a), a))),
      Pair("tail", gen(Arrow(listType(a), listType(a)))),
*/
      Pair("fix", gen(Arrow(Arrow(a, a), a)))
    )
  }

  trait MiniMLParsers extends CharParsers {

    /** whitespace */
    def whitespace = rep{chr(' ') ||| chr('\t') ||| chr('\n')}

    /** A given character, possible preceded by whitespace */
    def wschr(ch: char) = whitespace &&& chr(ch)

    def isLetter = (c: char) => Character.isLetter(c)
    def isLetterOrDigit: char => boolean = Character.isLetterOrDigit

    /** identifiers or keywords */
    def id: Parser[String] =
      for (
        c: char <- rep(chr(' ')) &&& chr(isLetter);
        cs: List[char] <- rep(chr(isLetterOrDigit))
      ) yield (c :: cs).mkString("", "", "")

    /** Non-keyword identifiers */
    def ident: Parser[String] =
      for (s <- id if s != "let" && s != "in") yield s

    /** term = '\' ident '.' term | term1 {term1} | let ident "=" term in term */
    def term: Parser[Term] = (
      ( for (
          _ <- wschr('\\');
          x <- ident;
          _ <- wschr('.');
          t <- term)
        yield Lam(x, t): Term )
      |||
      ( for (
          letid <- id if letid == "let"; 
          x <- ident;
          _ <- wschr('=');
          t <- term;
          inid <- id; if inid == "in";
          c <- term)
        yield Let(x, t, c) )
      |||
      ( for (
          t <- term1;
          ts <- rep(term1))
        yield (t /: ts)((f, arg) => App(f, arg)) )
    )

    /** term1 = ident | '(' term ')' */
    def term1: Parser[Term] = (
      ( for (s <- ident)
        yield Var(s): Term )
      |||
      ( for (
          _ <- wschr('(');
          t <- term;
          _ <- wschr(')'))
        yield t )
    )

    /** all = term ';' */
    def all: Parser[Term] =
      for (
        t <- term;
        _ <- wschr(';'))
      yield t
  }

  class ParseString(s: String) extends Parsers {
    type inputType = int
    val input = 0
    def any = new Parser[char] {
      def apply(in: int): Parser[char]#Result =
        if (in < s.length()) Some(Pair(s charAt in, in + 1)) else None
    }
  }

  def showType(e: Term): String =
    try {
      typeInfer.typeOf(predefined.env, e).toString()
    }
    catch {
      case typeInfer.TypeError(msg) =>
        "\n cannot type: " + typeInfer.current +
        "\n reason: " + msg
    }

  def main(args: Array[String]) {
    Console.println(
      if (args.length == 1) {
        val ps = new ParseString(args(0)) with MiniMLParsers
        ps.all(ps.input) match {
          case Some(Pair(term, _)) =>
            "" + term + ": " + showType(term)
          case None =>
            "syntax error"
        }
      }
      else
        "usage: java examples.typeinf <expr-string>"
    )
  }

}
