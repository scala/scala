object Test extends App {

  // For these methods, the compiler emits calls to BoxesRuntime.equalsNumNum/equalsNumChar/equalsNumObject directly

  def numNum(a: java.lang.Number, b: java.lang.Number) = assert(a == b)
  def numChar(a: java.lang.Number, b: java.lang.Character) = assert(a == b)
  def numObject(a: java.lang.Number, b: java.lang.Object) = assert(a == b)

  // The compiler doesn't use equalsCharObject directly, but still adding an example for completeness

  def charObject(a: java.lang.Character, b: java.lang.Object) = assert(a == b)

  numNum(new Integer(1), new Integer(1))
  numChar(new Integer(97), new Character('a'))
  numObject(new Integer(1), new Integer(1))
  numObject(new Integer(97), new Character('a'))

  charObject(new Character('a'), new Integer(97))
}
