package scala.util.regexp ;

/** this runtime exception is thrown if an attempt to instantiate a
  * syntactically incorrect expression is detected */
class SyntaxErrorExpression(e: String)
  extends java.lang.RuntimeException(e);
