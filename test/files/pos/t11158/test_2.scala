// interface CharSeqJava { int length(); }

trait CharSeqScala { def length: Int }

class A extends CharSeqScala with CharSeqJava {
  // since there's a matching java-defined method, we convert the NullaryMethodType we would normally assign to this symbol to a MethodType(Nil, _)....
  // so we can override toString more easily :roll-eyes:
  def length: Int = 1
  def length_=(value: Int): Unit = {}
}

class Test {
  (new A).length = 1
  (new A).length += 1
}
