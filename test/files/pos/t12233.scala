
trait TypeClass[T]
class Hehe[T: TypeClass](i: Int, j: Int) {
  def this(i: Int)(implicit j: Int) = this(i, j)
}

/* was
test/files/pos/t12233.scala:4: error: too many arguments (found 2, expected 1) for constructor Hehe: (implicit evidence$1: TypeClass[T]): Hehe[T]
  def this(i: Int)(implicit j: Int) = this(i, j)
                                      ^
1 error
 */
