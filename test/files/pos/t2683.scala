class A
class B extends A

object Test {
  val c: Class[_ <: A] = Class.forName("B").asSubclass(classOf[A])
  val x: Option[Class[_ <: A]] = Some(3).map { case _ => c }
}
