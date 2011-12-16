trait SI_5054_q7 {
  /**
   * The full definition, either used with an implicit value or with an explicit one.
   *
   * Some more explanation on implicits...
   *
   * @param lost a lost parameter
   * @return some integer
   * @usecase def test1(): Int
   *
   * This takes the implicit value in scope.
   *
   * Example: `test1()`
   * 
   * @usecase def test2(explicit: Int): Int
   * 
   * This takes the explicit value passed.
   *
   * Example: `test2(3)`
   */
  def test(implicit lost: Int): Int
}
