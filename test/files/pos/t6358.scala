class L(val t: Int) extends AnyVal {
   def lazyString = {
      lazy val x = t.toString
      () => x
   }
}
