class Test {
   def m = {
     case class CC(c: Int)
     if ("".isEmpty) {
       object CC { def inner = 42}
     }
     CC.inner
   }
  def n = {
    object CC { val outer = 42 }
    if ("".isEmpty) {
      case class CC(c: Int)
      CC(0).c
      CC.outer
    }
  }
}
