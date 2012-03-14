object SpecializationAbstractOverride {

  trait A[@specialized(Int) T] {                   def foo(t: T) }
  trait B extends A[Int]       {                   def foo(t: Int) { println("B.foo") } }
  trait M extends B            { abstract override def foo(t: Int) { super.foo(t) ; println ("M.foo") } }
  object C extends B with M 

  object D extends B           {          override def foo(t: Int) { super.foo(t); println("M.foo") } }
  
  def main(args: Array[String]) {
    D.foo(42) // OK, prints B.foo M.foo
    C.foo(42) // StackOverflowError
  }
}

