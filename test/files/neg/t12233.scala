
trait TypeClass[T]
class Hehe[T: TypeClass](i: Int, j: Int) {
  def this(i: Int)(implicit hehe: TypeClass[T], j: Int) = this(i, j)
}

/* was
t12233.scala:4: error: too many arguments (found 3, expected 1) for constructor Hehe: (implicit evidence$1: TypeClass[T]): Hehe[T]
  def this(i: Int)(implicit hehe: TypeClass[T], j: Int) = this(i, j)
                                                          ^
1 error
 * now
t12233.scala:4: error: ambiguous implicit values:
 both value hehe of type TypeClass[T]
 and value evidence$2 of type TypeClass[T]
 match expected type TypeClass[T]
  def this(i: Int)(implicit hehe: TypeClass[T], j: Int) = this(i, j)
                                                          ^
1 error
 */
