//############################################################################
// Programmation IV - 2002 - Week 13
//############################################################################

class Tokenizer(s: String, delimiters: String) extends Iterator[String] {

  private var i = 0;

  def isDelimiter(ch: Char) = {
    var i = 0;
    while (i < delimiters.length() && delimiters.charAt(i) != ch) { i = i + 1 }
    i < delimiters.length()
  }

  def hasNext: Boolean = {
    while (i < s.length() && s.charAt(i) <= ' ') { i = i + 1 }
    i < s.length()
  }

  def next: String =
    if (hasNext) {
      val start = i;
      var ch = s.charAt(i); i = i + 1;
      if (isDelimiter(ch)) ch.toString()
      else {
	while (i < s.length() &&
	       s.charAt(i) > ' ' &&
	       !isDelimiter(s.charAt(i))){ i = i + 1 }
	s.substring(start, i)
      }
    } else "";

}

object Terms {

  val debug = false;

  trait Term {
    def map(s: Subst): Term;
    def tyvars: List[String];
  }

  case class Binding(name: String, term: Term) {
    term match { case Var(n) if (name == n) => error("bad binding") case _ => () }
    override def toString() = name + " = " + term;
  }

  type Subst = List[Binding];

  def lookup(s: Subst, name: String): Option[Term] = s match {
    case List() => None
    case b :: s1 => if (name == b.name) Some(b.term) else lookup(s1, name)
  }

  case class Var(a: String) extends Term {
    override def toString() = a;
    def map(s: Subst): Term = lookup(s, a) match {
      case Some(b) => b map s
      case None => this;
    }
    def tyvars = List(a);
  }

  case class Con(a: String, ts: List[Term]) extends Term {
    override def toString() =
      a + (if (ts.isEmpty) "" else ts.mkString("(", ",", ")"));
    def map(s: Subst): Term = Con(a, ts map (t => t map s));
    def tyvars = (ts flatMap (t => t.tyvars)).distinct;
  }

  private var count = 0;
  def newVar(prefix: String) = { count = count + 1; Var(prefix + count) }

  val NoTerm = Con("<none>", List());

  def unify1(x: Term, y: Term, s: Subst): Option[Subst] = Pair(x, y) match {
    case Pair(Var(a), Var(b)) if (a == b) =>
      Some(s)
    case Pair(Var(a), _) => lookup(s, a) match {
      case Some(x1) => unify(x1, y, s)
      case None => if (y.tyvars contains a) None else Some(Binding(a, y) :: s)
    }
    case Pair(_, Var(b)) => lookup(s, b) match {
      case Some(y1) => unify(x, y1, s)
      case None => if (x.tyvars contains b) None else Some(Binding(b, x) :: s)
    }
    case Pair(Con(a, xs), Con(b, ys)) if (a == b) =>
      unify(xs, ys, s)
    case _ => None
  }

  def unify(x: Term, y: Term, s: Subst): Option[Subst] = {
    val ss = unify1(x, y, s);
    if (debug) Console.println("unify " + x + " with " + y + " = " + ss);
    ss
  }

  def unify(xs: List[Term], ys: List[Term], s: Subst): Option[Subst] = Pair(xs, ys) match {
    case Pair(List(), List()) => Some(s)
    case Pair(x :: xs1, y :: ys1) =>
      unify(x, y, s) match {
	case Some(s1) => unify(xs1, ys1, s1)
	case None => None
      }
    case _ => None
  }
}

import Terms._;

object Programs {

  case class Clause(lhs: Term, rhs: List[Term]) {
    def tyvars =
      (lhs.tyvars ::: (rhs flatMap (t => t.tyvars))).distinct;
    def newInstance = {
      var s: Subst = List();
      for (val a <- tyvars) { s = Binding(a, newVar(a)) :: s }
      Clause(lhs map s, rhs map (t => t map s))
    }
    override def toString() =
      lhs.toString() + " :- " + rhs.mkString("", ",", "") + ".";
  }

  def list2stream[a](xs: List[a]): Stream[a] = xs match {
    case List() => Stream.empty
    case x :: xs1 => Stream.cons(x, list2stream(xs1))
  }
  def option2stream[a](xo: Option[a]): Stream[a] = xo match {
    case None => Stream.empty
    case Some(x) => Stream.cons(x, Stream.empty)
  }

  def solve(query: List[Term], clauses: List[Clause]): Stream[Subst] = {

    def solve2(query: List[Term], s: Subst): Stream[Subst] = query match {
      case List() =>
	Stream.cons(s, Stream.empty)
      case Con("not", qs) :: query1 =>
	if (solve1(qs, s).isEmpty) Stream.cons(s, Stream.empty)
	else Stream.empty
      case q :: query1 =>
	for (val clause <- list2stream(clauses);
	     val s1 <- tryClause(clause.newInstance, q, s);
	     val s2 <- solve1(query1, s1)) yield s2
    }

    def solve1(query: List[Term], s: Subst): Stream[Subst] = {
      val ss = solve2(query, s);
      if (debug) Console.println("solved " + query + " = " + ss);
      ss
    }

    def tryClause(c: Clause, q: Term, s: Subst): Stream[Subst] = {
      if (debug) Console.println("trying " + c);
      for (val s1 <- option2stream(unify(q, c.lhs, s));
	   val s2 <- solve1(c.rhs, s1)) yield s2;
    }

    solve1(query, List())
  }
}

import Programs._;

class Parser(s: String) {
  val it = new Tokenizer(s, "(),.?");

  var token: String = it.next;

  def syntaxError(msg: String): Unit = error(msg + ", but " + token + " found");

  def rep[a](p: => a): List[a] = {
    val t = p;
    if (token == ",") { token = it.next; t :: rep(p) } else List(t)
  }

  def constructor: Term = {
    val a = token;
    token = it.next;
    Con(a,
	if (token equals "(") {
	  token = it.next;
	  val ts: List[Term] = if (token equals ")") List() else rep(term);
	  if (token equals ")") token = it.next else syntaxError("`)' expected");
	  ts
	} else List())
  }

  def term: Term = {
    val ch = token.charAt(0);
    if ('A' <= ch && ch <= 'Z') { val a = token; token = it.next; Var(a) }
    else if (it.isDelimiter(ch)) { syntaxError("term expected"); null }
    else constructor
  }

  def line: Clause = {
    val result =
      if (token equals "?") {
        token = it.next;
        Clause(NoTerm, rep(constructor));
      } else {
	Clause(
          constructor,
          if (token equals ":-") { token = it.next; rep(constructor) } else List())
      }
    if (token equals ".") token = it.next else syntaxError("`.' expected");
    result
  }

  def all: List[Clause] = if (token equals "") List() else line :: all;
}

object Prolog {

  def processor: String => Unit = {
    var program: List[Clause] = List();
    var solutions: Stream[Subst] = Stream.empty;
    var tvs: List[String] = List();
    { input =>
      new Parser(input).all foreach { c =>
	if (c.lhs == NoTerm) {
	  c.rhs match {
	    case List(Con("more", List())) =>
              solutions = solutions.tail;
	    case _ =>
              solutions = solve(c.rhs, program);
	      tvs = c.tyvars;
          }
	  if (solutions.isEmpty) {
            Console.println("no")
	  } else {
	    val s: Subst = solutions.head
	      .filter(b => tvs contains b.name)
	      .map(b => Binding(b.name, b.term map solutions.head))
              .reverse;
	    if (s.isEmpty) Console.println("yes")
	    else Console.println(s);
          }
	} else {
	  program = program ::: List(c);
	}
      }
    }
  }

  def process(code: String) = processor(code);
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    Prolog.process(
      "sujet(jean).\n" +
      "sujet(marie).\n" +
      "verbe(mange).\n" +
      "verbe(dort).\n" +
      "article(le).\n" +
      "article(la).\n" +
      "adjectif(grand).\n" +
      "adjectif(belle).\n" +
      "nom(table).\n" +
      "nom(cheval).\n" +

      "complement(A,D,N) :- article(A), adjectif(D), nom(N).\n" +
      "phrase(S,V,A,D,N) :- sujet(S), verbe(V), complement(A,D,N).\n" +

      "?phrase(S,V,A,D,N).\n" + "?more.\n"
    );
    Console.println;

    Prolog.process(
      "sujet(jean).\n" +
      "sujet(marie).\n" +
      "verbe(mange).\n" +
      "verbe(dort).\n" +
      "article(le,m).\n" +
      "article(la,f).\n" +
      "adjectif(grand,m).\n" +
      "adjectif(belle,f).\n" +
      "nom(table,f).\n" +
      "nom(cheval,m).\n" +

      "complement(A,D,N) :- article(A,G), adjectif(D,G), nom(N,G).\n" +
      "phrase(S,V,A,D,N) :- sujet(S), verbe(V), complement(A,D,N).\n" +

      "?phrase(S,V,A,D,N).\n" + "?more.\n"
    );
    Console.println;

    Prolog.process(
      "sujet(jean).\n" +
      "sujet(marie).\n" +
      "verbe(mange).\n" +
      "verbe(dort).\n" +
      "article(le,m).\n" +
      "article(la,f).\n" +
      "adjectif(grand,m).\n" +
      "adjectif(belle,f).\n" +
      "nom(table,f).\n" +
      "nom(cheval,m).\n" +

      "adjectifs(nil,G).\n" +
      "adjectifs(cons(A1,nil),G) :- adjectif(A1,G).\n" +
      "adjectifs(cons(A1,cons(A2,nil)),G) :- adjectif(A1,G),adjectif(A2,G).\n"+
      "complement(A,D,N) :- article(A,G), adjectifs(D,G), nom(N,G).\n" +
      "phrase(S,V,A,D,N) :- sujet(S), verbe(V), complement(A,D,N).\n" +

      "?phrase(S,V,A,D,N).\n" + "?more.\n" + "?more.\n" + "?more.\n" +

      "?phrase(jean,mange,le,nil,cheval).\n" +
      "?phrase(jean,mange,le,cons(grand,nil),cheval).\n" +
      "?phrase(jean,mange,le,cons(grand,nil),table).\n"
    );
    Console.println;

    ()
  }
}

//############################################################################
