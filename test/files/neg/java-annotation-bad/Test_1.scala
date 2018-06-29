object Test {
  import ann._

  // ok
  @Ann_0(Array(new Ann_0.N, new Ann_0.N))
  class A

  // ok
  @Ann_1(Array(new Ann_1.N, new Ann_1.N))
  class B

  val a: Ann_0 = new Ann_0          // nok
  val b: Ann_0 = new Ann_0(Array()) // nok
  val c: Ann_1 = new Ann_1          // nok
  val d: Ann_1 = new Ann_1(Array()) // nok

  import scala.annotation._, java.lang.{annotation => jla}
  val e: Annotation         = a // nok
  val f: Annotation         = c // nok
  val g: StaticAnnotation   = a // nok
  val h: StaticAnnotation   = c // nok
  val i: ConstantAnnotation = a // nok
  val j: ConstantAnnotation = c // nok
  val k: jla.Annotation     = a // ok
  val l: jla.Annotation     = c // ok

  val m = new Ann_0 { val annotationType = classOf[Ann_0] } // ok
  val n = new Ann_1 { val annotationType = classOf[Ann_1] } // ok

}