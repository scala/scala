object Test {

  def eat(foods: List["food"]): Unit = println(s"ate ${foods.size} foods")

  eat("food" :: Nil)
  eat(Nil.::("food"))

}
