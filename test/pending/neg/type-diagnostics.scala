object TooManyParens {
  def f = Map(1 -> 2).keySet()
  //
  // Confusion reigns!
  //
  // work/a.scala:27: error: not enough arguments for method apply: (elem: Int)Boolean in trait SetLike.
  // Unspecified value parameter elem.
  //   def f = Map(1 -> 2).keySet()
  //                             ^
  
}
