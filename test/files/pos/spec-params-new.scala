import scala.reflect.{ClassTag, classTag}

class Foo[@specialized A: ClassTag] {

  // conflicting in bounds, expect a normalized member calling m
  // and bridge + implementation in specialized subclasses
  // and overloads here according to specialization on A
  def m1[@specialized B <: A](x: B, y: A) =
    goal(x)

  // conflicting, unsolvable, expect a warning
  def m2[@specialized B <: String](x: B) = x.concat("a")

  // conflicting in bounds, no mention of other spec members
  // expect an overload here plus implementation in
  // compatible specialized subclasses
  def m3[@specialized B >: A](x: B) = ()

  // non-conflicting, expect a normalized overload implementation here
  def m4[@specialized T, U <: Ordered[T]](x: T, y: U) = ()

  // non-conflicting, expect a normalized overload implementation here
  def m5[@specialized B](x: B) = x

  // non-conflicting, expect a normalized implementation here
  // and specialized implementations for all expansions in specialized subclasses
  def m6[@specialized B](x: B, y: A) =
    goal(y)

  def goal(x: A) = {
    val xs = new Array[A](1)
    xs(0) = x
  }
}
