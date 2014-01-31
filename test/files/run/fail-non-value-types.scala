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

  def cil      = typeOf[CompletelyIndependentList[Int]]
  def map      = cil.member(TermName("map")).asMethod
  def distinct = cil.member(TermName("distinct")).asMethod

  def main(args: Array[String]): Unit = {
    // Need the assert in there to fail.
    // expectFailure(println(tcon[CompletelyIndependentList[Int]](map)))
    // expectFailure(tcon[CompletelyIndependentList[Int]](distinct))

    // Why is the first map signature printing showing an
    // uninitialized symbol?
    //
    // [B <: <?>, That <: <?>](f: <?>)(implicit cbf: <?>)That
    //

    println(map.info)
    println(map.infoIn(cil))
    println(distinct.info)
    if (failed) sys.exit(1)
  }
}
