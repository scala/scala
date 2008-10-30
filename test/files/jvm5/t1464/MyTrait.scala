trait MyTrait {
  type K
  def findChildByClass[T <: K with MyTrait]: Unit

}
