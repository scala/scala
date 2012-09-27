import scala.reflect.runtime.universe._

class ImaginaryCanBuildFrom[-From, -Elem, +To]
class CompletelyIndependentList[+A] {
  type Repr <: CompletelyIndependentList[A]
  def map[B, That](f: A => B)(implicit cbf: ImaginaryCanBuildFrom[Repr, B, That]): That = ???
  def distinct(): CompletelyIndependentList[A] = ???
}

object Test {
  var failed = false
  def expectFailure[T](body: => T): Boolean = {
    try { val res = body ; failed = true ; println(res + " failed to fail.") ; false }
    catch { case _: AssertionError => true }
  }

  /** Attempt to use a method type as a type argument - expect failure. */
  def tcon[T: TypeTag](args: Type*) = appliedType(typeOf[T].typeConstructor, args.toList)

  val map      = typeOf[CompletelyIndependentList[Int]].member("map": TermName).typeSignature
  val distinct = typeOf[CompletelyIndependentList[Int]].member("distinct": TermName).typeSignature

  def main(args: Array[String]): Unit = {
    expectFailure(println(tcon[CompletelyIndependentList[Int]](map)))
    expectFailure(tcon[CompletelyIndependentList[Int]](distinct))
    println(map)
    println(distinct)
    if (failed) sys.exit(1)
  }
}
