object Test {
  def ltr[A](implicit ev: (Int, A) =:= (Int, String)) = null
  def rtl[A](implicit ev: (Int, String) =:= (Int, A)) = null
  ltr
  rtl
}
