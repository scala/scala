


import runtime.ScalaRunTime._


class Generic[T](a: Array[T]) {
  def apply() = a(0)
}


class Spec[@specialized(AnyRef) T](a: Array[T]) {
  def apply() = a(0)
}


object Test {

  def main(args: Array[String]) {
    val len = 50

    testSpec(new Array[String](len))
    println(arrayApplyCount)

    (new Spec(new Array[String](len)))()
    println(arrayApplyCount)

    testGeneric(new Array[String](len))
    println(arrayApplyCount)

    (new Generic(new Array[String](len)))()
    println(arrayApplyCount)
  }

  def testGeneric[T](a: Array[T]) = {
    var i = 0
    var sum = 0
    while (i < a.length) {
      sum += (if (a(i) != null) 1 else 0)
      i += 1
    }
    sum
  }

  def testSpec[@specialized(AnyRef) T](a: Array[T]) = {
    var i = 0
    var sum = 0
    while (i < a.length) {
      sum += (if (a(i) != null) 1 else 0)
      i += 1
    }
    sum
  }

}
