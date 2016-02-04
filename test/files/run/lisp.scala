//############################################################################
// Lisp interpreter
//############################################################################

//############################################################################
// Lisp Scanner

class LispTokenizer(s: String) extends Iterator[String] {
  private var i = 0;
  private def isDelimiter(ch: Char) = ch <= ' ' || ch == '(' || ch == ')'
  def hasNext: Boolean = {
    while (i < s.length() && s.charAt(i) <= ' ') i += 1
    i < s.length()
  }
  def next: String =
    if (hasNext) {
      val start = i
      if (isDelimiter(s charAt i)) i += 1
      else
        do i = i + 1
        while (!isDelimiter(s charAt i))
      s.substring(start, i)
    } else sys.error("premature end of string")
}

//############################################################################
// Lisp Interface

trait Lisp {
  type Data

  def string2lisp(s: String): Data
  def lisp2string(s: Data): String

  def evaluate(d: Data): Data
  // !!! def evaluate(s: String): Data = evaluate(string2lisp(s))
  def evaluate(s: String): Data
}

//############################################################################
// Lisp Implementation Using Case Classes

object LispCaseClasses extends Lisp {

  import List.range

  trait Data {
    def elemsToString(): String = toString();
  }
  case class CONS(car: Data, cdr: Data) extends Data {
    override def toString() = "(" + elemsToString() + ")";
    override def elemsToString() = car.toString() + (cdr match {
      case NIL() => ""
      case _     => " " + cdr.elemsToString();
    })
  }
  case class NIL() extends Data { // !!! use case object
    override def toString() = "()";
  }
  case class SYM(name: String) extends Data {
    override def toString() = name;
  }
  case class NUM(x: Int) extends Data {
    override def toString() = x.toString();
  }
  case class STR(x: String) extends Data {
    override def toString() = "\"" + x + "\"";
  }
  case class FUN(f: List[Data] => Data) extends Data {
    override def toString() = "<fn>";
  }

  def list(): Data =
    NIL();
  def list(x0: Data): Data =
    CONS(x0, NIL());
  def list(x0: Data, x1: Data): Data =
    CONS(x0, list(x1));
  def list(x0: Data, x1: Data, x2: Data): Data =
    CONS(x0, list(x1, x2));
  def list(x0: Data, x1: Data, x2: Data, x3: Data): Data =
    CONS(x0, list(x1, x2, x3));
  def list(x0: Data, x1: Data, x2: Data, x3: Data, x4: Data): Data =
    CONS(x0, list(x1, x2, x3, x4));
  def list(x0: Data, x1: Data, x2: Data, x3: Data, x4: Data, x5: Data): Data =
    CONS(x0, list(x1, x2, x3, x4, x5));
  def list(x0: Data, x1: Data, x2: Data, x3: Data, x4: Data, x5: Data,
   x6: Data): Data =
    CONS(x0, list(x1, x2, x3, x4, x5, x6));
  def list(x0: Data, x1: Data, x2: Data, x3: Data, x4: Data, x5: Data,
   x6: Data, x7: Data): Data =
    CONS(x0, list(x1, x2, x3, x4, x5, x6, x7));
  def list(x0: Data, x1: Data, x2: Data, x3: Data, x4: Data, x5: Data,
   x6: Data, x7: Data, x8: Data): Data =
    CONS(x0, list(x1, x2, x3, x4, x5, x6, x7, x8));
  def list(x0: Data, x1: Data, x2: Data, x3: Data, x4: Data, x5: Data,
   x6: Data, x7: Data, x8: Data, x9: Data): Data =
    CONS(x0, list(x1, x2, x3, x4, x5, x6, x7, x8, x9));

  var curexp: Data = null
  var trace: Boolean = false
  var indent: Int = 0

  def lispError[a](msg: String): a =
    sys.error("error: " + msg + "\n" + curexp);

  trait Environment {
    def lookup(n: String): Data;
    def extendRec(name: String, expr: Environment => Data) =
      new Environment {
        def lookup(n: String): Data =
          if (n == name) expr(this) else Environment.this.lookup(n);
      }
    def extend(name: String, v: Data) = extendRec(name, (env1 => v));
  }
  val EmptyEnvironment = new Environment {
    def lookup(n: String): Data = lispError("undefined: " + n);
  }

  def toList(x: Data): List[Data] = x match {
    case NIL() => List()
    case CONS(y, ys) => y :: toList(ys)
    case _ => lispError("malformed list: " + x);
  }

  def toBoolean(x: Data) = x match {
    case NUM(0) => false
    case _ => true
  }

  def normalize(x: Data): Data = x match {
    case CONS(SYM("def"),
           CONS(CONS(SYM(name), args), CONS(body, CONS(expr, NIL())))) =>
      normalize(list(SYM("def"),
        SYM(name), list(SYM("lambda"), args, body), expr))
    case CONS(SYM("cond"), CONS(CONS(SYM("else"), CONS(expr, NIL())),NIL())) =>
      normalize(expr)
    case CONS(SYM("cond"), CONS(CONS(test, CONS(expr, NIL())), rest)) =>
      normalize(list(SYM("if"), test, expr, CONS(SYM("cond"), rest)))
    case CONS(h, t) => CONS(normalize(h), normalize(t))
    case _ => x
  }

  def eval(x: Data, env: Environment): Data = {
    val prevexp = curexp;
    curexp = x;
    if (trace) {
      for (x <- range(1, indent)) Console.print(" ");
      Console.println("===> " + x);
      indent = indent + 1;
    }
    val result = eval1(x, env);
    if (trace) {
      indent = indent - 1;
      for (x <- range(1, indent)) Console.print(" ");
      Console.println("<=== " + result);
    }
    curexp = prevexp;
    result
  }

  def eval1(x: Data, env: Environment): Data = x match {
    case SYM(name) =>
      env lookup name
    case CONS(SYM("def"), CONS(SYM(name), CONS(y, CONS(z, NIL())))) =>
      eval(z, env.extendRec(name, (env1 => eval(y, env1))))
    case CONS(SYM("val"), CONS(SYM(name), CONS(y, CONS(z, NIL())))) =>
      eval(z, env.extend(name, eval(y, env)))
    case CONS(SYM("lambda"), CONS(params, CONS(y, NIL()))) =>
      mkLambda(params, y, env)
    case CONS(SYM("if"), CONS(c, CONS(t, CONS(e, NIL())))) =>
      if (toBoolean(eval(c, env))) eval(t, env) else eval(e, env)
    case CONS(SYM("quote"), CONS(x, NIL())) =>
      x
    case CONS(y, xs) =>
      apply(eval(y, env), toList(xs) map (x => eval(x, env)))
    case NUM(_) => x
    case STR(_) => x
    case FUN(_) => x
    case _ =>
      lispError("illegal term")
    }

  def apply(fn: Data, args: List[Data]): Data = fn match {
    case FUN(f) => f(args);
    case _ => lispError("application of non-function: " + fn);
  }

  def mkLambda(params: Data, expr: Data, env: Environment): Data = {

    def extendEnv(env: Environment,
                  ps: List[String], args: List[Data]): Environment =
      (ps, args) match {
        case (List(), List()) =>
          env
        case (p :: ps1, arg :: args1) =>
          extendEnv(env.extend(p, arg), ps1, args1)
        case _ =>
          lispError("wrong number of arguments")
      }

    val ps: List[String] = toList(params) map {
      case SYM(name) => name
      case _ => sys.error("illegal parameter list");
    }

    FUN(args => eval(expr, extendEnv(env, ps, args)))
  }

  val globalEnv = EmptyEnvironment
    .extend("=", FUN({
      case List(NUM(arg1),NUM(arg2)) => NUM(if (arg1 == arg2) 1 else 0)
      case List(STR(arg1),STR(arg2)) => NUM(if (arg1 == arg2) 1 else 0)}))
    .extend("+", FUN({
      case List(NUM(arg1),NUM(arg2)) => NUM(arg1 + arg2)
      case List(STR(arg1),STR(arg2)) => STR(arg1 + arg2)}))
    .extend("-", FUN({
      case List(NUM(arg1),NUM(arg2)) => NUM(arg1 - arg2)}))
    .extend("*", FUN({
      case List(NUM(arg1),NUM(arg2)) => NUM(arg1 * arg2)}))
    .extend("/", FUN({
      case List(NUM(arg1),NUM(arg2)) => NUM(arg1 / arg2)}))
    .extend("car", FUN({
      case List(CONS(x, xs)) => x}))
    .extend("cdr", FUN({
      case List(CONS(x, xs)) => xs}))
    .extend("null?", FUN({
      case List(NIL()) => NUM(1)
      case _ => NUM(0)}))
    .extend("cons", FUN({
      case List(x, y) => CONS(x, y)}));

  def evaluate(x: Data): Data = eval(normalize(x), globalEnv);
  def evaluate(s: String): Data = evaluate(string2lisp(s));

  def string2lisp(s: String): Data = {
    val it = new LispTokenizer(s);
    def parse(token: String): Data = {
      if (token == "(") parseList
      else if (token == ")") sys.error("unbalanced parentheses")
      else if ('0' <= token.charAt(0) && token.charAt(0) <= '9')
        NUM(token.toInt)
      else if (token.charAt(0) == '\"' && token.charAt(token.length()-1)=='\"')
        STR(token.substring(1,token.length() - 1))
      else SYM(token)
    }
    def parseList: Data = {
      val token = it.next;
      if (token == ")") NIL() else CONS(parse(token), parseList)
    }
    parse(it.next)
  }

  def lisp2string(d: Data): String = d.toString();
}

//############################################################################
// Lisp Implementation Using Any

object LispAny extends Lisp {

  import List._;

  type Data = Any;

  case class Lambda(f: List[Data] => Data);

  var curexp: Data = null;
  var trace: Boolean = false;
  var indent: Int = 0;

  def lispError[a](msg: String): a =
    sys.error("error: " + msg + "\n" + curexp);

  trait Environment {
    def lookup(n: String): Data;
    def extendRec(name: String, expr: Environment => Data) =
      new Environment {
        def lookup(n: String): Data =
          if (n == name) expr(this) else Environment.this.lookup(n);
      }
    def extend(name: String, v: Data) = extendRec(name, (env1 => v));
  }
  val EmptyEnvironment = new Environment {
    def lookup(n: String): Data = lispError("undefined: " + n);
  }

  def asList(x: Data): List[Data] = x match {
    case y: List[_] => y
    case _ => lispError("malformed list: " + x)
  }

  def asInt(x: Data): Int = x match {
    case y: Int => y
    case _ => lispError("not an integer: " + x)
  }

  def asString(x: Data): String = x match {
    case y: String => y
    case _ => lispError("not a string: " + x)
  }

  def asBoolean(x: Data): Boolean = x != 0

  def normalize(x: Data): Data = x match {
    case 'and :: x :: y :: Nil =>
      normalize('if :: x :: y :: 0 :: Nil)
    case 'or :: x :: y :: Nil =>
      normalize('if :: x :: 1 :: y :: Nil)
    case 'def :: (name :: args) :: body :: expr :: Nil =>
      normalize('def :: name :: ('lambda :: args :: body :: Nil) :: expr :: Nil)
    case 'cond :: ('else :: expr :: Nil) :: rest =>
        normalize(expr);
    case 'cond :: (test :: expr :: Nil) :: rest =>
	normalize('if :: test :: expr :: ('cond :: rest) :: Nil)
    case 'cond :: 'else :: expr :: Nil =>
      normalize(expr)
    case h :: t =>
      normalize(h) :: asList(normalize(t))
    case _ =>
      x
  }

  def eval(x: Data, env: Environment): Data = {
    val prevexp = curexp;
    curexp = x;
    if (trace) {
      for (x <- range(1, indent)) Console.print(" ");
      Console.println("===> " + x);
      indent += 1;
    }
    val result = eval1(x, env);
    if (trace) {
      indent -= 1;
      for (x <- range(1, indent)) Console.print(" ");
      Console.println("<=== " + result);
    }
    curexp = prevexp;
    result
  }

  def eval1(x: Data, env: Environment): Data = x match {
    case Symbol(name) =>
      env lookup name
    case 'def :: Symbol(name) :: y :: z :: Nil =>
      eval(z, env.extendRec(name, (env1 => eval(y, env1))))
    case 'val :: Symbol(name) :: y :: z :: Nil =>
      eval(z, env.extend(name, eval(y, env)))
    case 'lambda :: params :: y :: Nil =>
      mkLambda(params, y, env)
    case 'if :: c :: y :: z :: Nil =>
      if (asBoolean(eval(c, env))) eval(y, env) else eval(z, env)
    case 'quote :: y :: Nil =>
      y
    case y :: z =>
      apply(eval(y, env), z map (x => eval(x, env)))
    case Lambda(_) => x
    case y: String => x
    case y: Int => x
    case y => lispError("illegal term")
  }

  def lisp2string(x: Data): String = x match {
    case Symbol(name) => name
    case Nil => "()"
    case y :: ys =>
      def list2string(xs: List[Data]): String = xs match {
        case List() => ""
        case y :: ys => " " + lisp2string(y) + list2string(ys)
      }
      "(" + lisp2string(y) + list2string(ys) + ")"
    case _ => if (x.isInstanceOf[String]) "\"" + x + "\""; else x.toString()
  }

  def apply(fn: Data, args: List[Data]): Data = fn match {
    case Lambda(f) => f(args);
    case _ => lispError("application of non-function: " + fn + " to " + args);
  }

  def mkLambda(params: Data, expr: Data, env: Environment): Data = {

    def extendEnv(env: Environment,
                  ps: List[String], args: List[Data]): Environment =
      (ps, args) match {
        case (List(), List()) =>
          env
        case (p :: ps1, arg :: args1) =>
          extendEnv(env.extend(p, arg), ps1, args1)
        case _ =>
          lispError("wrong number of arguments")
      }

    val ps: List[String] = asList(params) map {
      case Symbol(name) => name
      case _ => sys.error("illegal parameter list");
    }

    Lambda(args => eval(expr, extendEnv(env, ps, args)))
  }

  val globalEnv = EmptyEnvironment
    .extend("=", Lambda{
      case List(arg1, arg2) => if(arg1 == arg2) 1 else 0})
    .extend("+", Lambda{
      case List(arg1: Int, arg2: Int) => arg1 + arg2
      case List(arg1: String, arg2: String) => arg1 + arg2})
    .extend("-", Lambda{
      case List(arg1: Int, arg2: Int) => arg1 - arg2})
    .extend("*", Lambda{
      case List(arg1: Int, arg2: Int) => arg1 * arg2})
    .extend("/", Lambda{
      case List(arg1: Int, arg2: Int) => arg1 / arg2})
    .extend("nil", Nil)
    .extend("cons", Lambda{
      case List(arg1, arg2) => arg1 :: asList(arg2)})
    .extend("car", Lambda{
      case List(x :: xs) => x})
    .extend("cdr", Lambda{
      case List(x :: xs) => xs})
    .extend("null?", Lambda{
      case List(Nil) => 1
      case _ => 0});

  def evaluate(x: Data): Data = eval(normalize(x), globalEnv);
  def evaluate(s: String): Data = evaluate(string2lisp(s));

  def string2lisp(s: String): Data = {
    val it = new LispTokenizer(s);
    def parse(token: String): Data = {
      if (token == "(") parseList
      else if (token == ")") sys.error("unbalanced parentheses")
      //else if (Character.isDigit(token.charAt(0)))
      else if (token.charAt(0).isDigit)
        token.toInt
      else if (token.charAt(0) == '\"' && token.charAt(token.length()-1)=='\"')
        token.substring(1,token.length() - 1)
      else Symbol(token)
    }
    def parseList: List[Data] = {
      val token = it.next;
      if (token == ")") Nil else parse(token) :: parseList
    }
    parse(it.next)
  }
}

//############################################################################
// List User

class LispUser(lisp: Lisp) {

  import lisp._;

  def evaluate(s: String) = lisp2string(lisp.evaluate(s));

  def run = {

    Console.println(string2lisp("(lambda (x) (+ (* x x) 1))").asInstanceOf[AnyRef]);
    Console.println(lisp2string(string2lisp("(lambda (x) (+ (* x x) 1))")));
    Console.println;

    Console.println("(    '(1 2 3)) = " + evaluate("     (quote(1 2 3))"));
    Console.println("(car '(1 2 3)) = " + evaluate("(car (quote(1 2 3)))"));
    Console.println("(cdr '(1 2 3)) = " + evaluate("(cdr (quote(1 2 3)))"));
    Console.println("(null? '(2 3)) = " + evaluate("(null? (quote(2 3)))"));
    Console.println("(null?    '()) = " + evaluate("(null?    (quote()))"));
    Console.println;

    Console.println("faculty(10) = " + evaluate(
      "(def (faculty n) " +
        "(if (= n 0) " +
          "1 " +
          "(* n (faculty (- n 1)))) " +
        "(faculty 10))"));
    Console.println("faculty(10) = " + evaluate(
      "(def (faculty n) " +
        "(cond " +
          "((= n 0) 1) " +
          "(else (* n (faculty (- n 1))))) " +
        "(faculty 10))"));
    Console.println("foobar = " + evaluate(
      "(def (foo n) " +
        "(cond " +
          "((= n 0) \"a\")" +
          "((= n 1) \"b\")" +
          "((= (/ n 2) 1) " +
            "(cond " +
              "((= n 2) \"c\")" +
              "(else    \"d\")))" +
          "(else " +
            "(def (bar m) " +
              "(cond " +
                "((= m 0) \"e\")" +
                "((= m 1) \"f\")" +
                "(else    \"z\"))" +
              "(bar (- n 4)))))" +
        "(val nil (quote ())" +
          "(val v1 (foo 0) " +
            "(val v2 (+ (foo 1) (foo 2)) " +
              "(val v3 (+ (+ (foo 3) (foo 4)) (foo 5)) " +
                "(val v4 (foo 6) " +
                  "(cons v1 (cons v2 (cons v3 (cons v4 nil))))))))))"));
    Console.println;
  }
}

//############################################################################
// Main

object Test {
  def main(args: Array[String]) {
    new LispUser(LispCaseClasses).run;
    new LispUser(LispAny).run;
    ()
  }
}

//############################################################################
