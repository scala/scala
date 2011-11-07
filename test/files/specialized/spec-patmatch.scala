class Foo[@specialized A] {
  def test(x: A) = println(x match {
   case _: Boolean => "bool"
   case _: Byte => "byte"
   case _: Short => "short"
   case _: Char => "char"
   case i: Int => "int"
   case l: Long => "long"
   case d: Double => "double"
   case e: Float => "float"
   case _ => "default"
 })
}

object Test {
 def test[@specialized A] (x: A) = println(x match {
   case _: Boolean => "bool"
   case _: Byte => "byte"
   case _: Short => "short"
   case _: Char => "char"
   case i: Int => "int"
   case l: Long => "long"
   case d: Double => "double"
   case e: Float => "float"
   case _ => "default"
 })

  def main(args: Array[String]) {
    test(true)
    test(42.toByte)
    test(42.toShort)
    test('b')
    test(42)
    test(42l)
    test(42.0)
    test(42.0f)
    test(new Object)

    println("object instantiations:")
    (new Foo).test(true)
    (new Foo).test(42.toByte)
    (new Foo).test(42.toShort)
    (new Foo).test('b')
    (new Foo).test(42)
    (new Foo).test(42l)
    (new Foo).test(42.0)
    (new Foo).test(42.0f)
    (new Foo).test(new Object)
    
    println(runtime.BoxesRunTime.integerBoxCount)
  }

}
