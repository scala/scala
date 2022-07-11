package tastytest

/** The purpose of this test source is to see what remains unforced when
 *  depending on only the hello method from Scala 2.
 *  If any of the other definitions are forced, they will reach the tuple bound
 *  and fail compilation.
 */
object TupleBounds {

  def hello = "hello"

  class SomeDef[T <: scala.Tuple] // test is unforced

  type SomeOtherDef = { // test is unforced
    type T <: scala.Tuple
    def foo: scala.Tuple
    val bar: scala.Tuple
  }

  type SomeBoundedDef[T <: scala.Tuple] // test is unforced

  class TupleAnnot(t: scala.Tuple) extends scala.annotation.Annotation // test is unforced

  @TupleAnnot((1, "abc", true)) // test is unforced
  def someAnnotated: scala.Tuple = ??? // test is unforced

}
