//> using options -Werror

trait F

class T {
  def t1 = "" == Some("").getOrElse(None) // used to warn incorrectly, because a RefinementClassSymbol is unrelated to String

  def a: T with Serializable = null
  def b: Serializable with T = null
  def t2 = "" == a // no warn, the implementation bails on intersection types
  def t3 = "" == b // no warn

  def t1(a: F, b: Product with F) = a == b // no warn
  def t2(a: F, b: F with Product) = a == b // no warn
  def t3(a: F with Product, b: F) = a == b // no warn
  def t4(a: Product with F, b: F) = a == b // no warn
}
