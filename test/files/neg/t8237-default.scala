// This test case was extracted from `names-defaults-neg.scala`
// It pinpoints an improvement an error message that results from
// a type inference failure
object Test extends App {
  test4(test4$default$1)

  def test4[T[P]](x: T[T[List[T[X forSome { type X }]]]]) = ???
  def test4$default$1[T[P]]: List[Int] = ???
}

/*
OLD:
 no type parameters for method test4: (x: T[T[List[T[X forSome { type X }]]]])Nothing exist so that it can be applied to arguments (List[Int])
 --- because ---
argument expression's type is not compatible with formal parameter type;
 found   : List[Int]
 required: ?T
  test4(test4$default$1)
  ^

NEW:

no type parameters for method test4: (x: T[T[List[T[X forSome { type X }]]]])Nothing exist so that it can be applied to arguments (List[Int])
 --- because ---
argument expression's type is not compatible with formal parameter type;
 found   : List[Int]
 required: ?T[?T[List[?T[X forSome { type X }]]]
  test4(test4$default$1)
*/
