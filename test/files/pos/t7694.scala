trait A
trait B

trait L[A2, B2 <: A2] {
  def bar(a: Any, b: Any) = 0
}

object Lub {
  // use named args transforms to include TypeTree(<lub.tpe>) in the AST before refchecks.
  def foo(a: L[_, _], b: Any) = 0

  foo(b = 0, a = if (true) (null: L[A, A]) else (null: L[B, B]))

  (if (true) (null: L[A, A]) else (null: L[B, B])).bar(b = 0, a = 0)
}

/*
The LUB ends up as:

TypeRef(
  TypeSymbol(
    abstract trait L#7038[A2#7039, B2#7040 <: A2#7039] extends AnyRef#2197

  )
  args = List(
    AbstractTypeRef(
      AbstractType(
        type _1#13680 >: A#7036 with B#7037 <: Object#1752
      )
    )
    AbstractTypeRef(
      AbstractType(
        type _2#13681 >: A#7036 with B#7037 <: Object#1752
      )
    )
  )
)

Note that type _2#13681 is *not* bound by _1#13680
*/
