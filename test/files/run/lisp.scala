//############################################################################
// Lisp interpreter
//############################################################################
// $Id$

import List._;

object Lisp {

  type Data = Any;

  case class Lambda(f: List[Data] => Data);

  var curexp: Data = null;
  var trace: boolean = false;
  var indent: int = 0;

  def lispError[a](msg: String): a =
    error("error: " + msg + "\n" + curexp);

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

  def asList(x: Data): List[Data] =
    if (x is List[Data]) x as List[Data]
    else lispError("malformed list: " + x);

  def asBoolean(x: Data): boolean =
    if (x == 0) false else true;

  def asInt(x: Data): int =
    if (x is int) x as int else lispError("not an integer: " + x);

  def asString(x: Data): String =
    if (x is java.lang.String) x as java.lang.String else lispError("not a string: " + x);

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
      for (val x <- range(1, indent)) do System.out.print(" ");
      System.out.println("===> " + x);
      indent = indent + 1;
    }
    val result = eval1(x, env);
    if (trace) {
      indent = indent - 1;
      for (val x <- range(1, indent)) do System.out.print(" ");
      System.out.println("<=== " + result);
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
    case y: int => x
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
    case _ => x.toString()
  }

  def apply(fn: Data, args: List[Data]): Data = fn match {
    case Lambda(f) => f(args);
    case _ => lispError("application of non-function: " + fn + " to " + args);
  }

  def mkLambda(params: Data, expr: Data, env: Environment): Data = {

    def extendEnv(env: Environment,
                  ps: List[String], args: List[Data]): Environment =
      Pair(ps, args) match {
        case Pair(List(), List()) =>
          env
        case Pair(p :: ps1, arg :: args1) =>
          extendEnv(env.extend(p, arg), ps1, args1)
        case _ =>
          lispError("wrong number of arguments")
      }

    val ps: List[String] = asList(params) map {
      case Symbol(name) => name
      case _ => error("illegal parameter list");
    }

    Lambda(args => eval(expr, extendEnv(env, ps, args)))
  }

  val globalEnv = EmptyEnvironment
    .extend("=", Lambda{
      case List(arg1, arg2) => if(arg1 == arg2) 1 else 0})
    .extend("+", Lambda{
      case List(arg1: int, arg2: int) => arg1 + arg2
      case List(arg1: String, arg2: String) => arg1 + arg2})
    .extend("-", Lambda{
      case List(arg1: int, arg2: int) => arg1 - arg2})
    .extend("*", Lambda{
      case List(arg1: int, arg2: int) => arg1 * arg2})
    .extend("/", Lambda{
      case List(arg1: int, arg2: int) => arg1 / arg2})
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
    def parseExpr(token: String): Data = {
      if (token == "(") parseList
      else if (token == ")") error("unbalanced parentheses")
      else if (Character.isDigit(token.charAt(0))) Integer.parseInt(token)
      else if (token.charAt(0) == '\"' && token.charAt(token.length()-1)=='\"')
        token.substring(1,token.length() - 1)
      else Symbol(token)
    }
    def parseList: List[Data] = {
      val token = it.next;
      if (token == ")") Nil else parseExpr(token) :: parseList
    }
    parseExpr(it.next)
  }
}

class LispTokenizer(s: String) extends Iterator[String] {
  private var i = 0;
  private def isDelimiter(ch: Char) = ch <= ' ' || ch == '(' || ch == ')';
  def hasNext: Boolean = {
    while (i < s.length() && s.charAt(i) <= ' ') { i = i + 1 }
    i < s.length()
  }
  def next: String =
    if (hasNext) {
      val start = i;
      var ch = s.charAt(i); i = i + 1;
      if (ch == '(') "("
      else if (ch == ')') ")"
      else {
        while (i < s.length() && !isDelimiter(s.charAt(i))){ i = i + 1 }
        s.substring(start, i)
      }
    } else error("premature end of string")
}

//############################################################################

object M0 {

  def test = {
    import Lisp._;
    System.out.println();
    System.out.println("(    '(1 2 3)) = " + evaluate("     (quote(1 2 3))"));
    System.out.println("(car '(1 2 3)) = " + evaluate("(car (quote(1 2 3)))"));
    System.out.println("(cdr '(1 2 3)) = " + evaluate("(cdr (quote(1 2 3)))"));
    System.out.println("(null? '(2 3)) = " + evaluate("(null? (quote(2 3)))"));
    System.out.println("(null?    '()) = " + evaluate("(null?    (quote()))"));
    System.out.println();
    System.out.println("faculty(10) = " + evaluate(
      "(def (faculty n) " +
        "(if (= n 0) " +
          "1 " +
          "(* n (faculty (- n 1)))) " +
        "(faculty 10))"));
    System.out.println("faculty(10) = " + evaluate(
      "(def (faculty n) " +
        "(cond " +
          "((= n 0) 1) " +
          "(else (* n (faculty (- n 1))))) " +
        "(faculty 10))"));
    System.out.println("foobar = " + evaluate(
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
    System.out.println();
  }
}

//############################################################################

object Test {
  def main(args: Array[String]): Unit = {
    System.out.println(Lisp.string2lisp("(lambda (x) (+ (* x x) 1))") as Object);
    System.out.println(Lisp.lisp2string(Lisp.string2lisp("(lambda (x) (+ (* x x) 1))")));
    M0.test;
    ()
  }
}

//############################################################################
