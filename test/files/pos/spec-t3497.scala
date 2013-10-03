abstract class A[T, @specialized U] {
    def score(state: T): U
}

object B extends A[ Array[Byte], Int ] {
  def score(state: Array[Byte]): Int = {
    var index = 0
    while (index < state.length) { // (index < 2) leads to the #2755 NullPointerException
      if (state(index) == 0) {
        return -1
      }
    }

    return 0
  }
}
