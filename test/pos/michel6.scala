object M {
   def f(x: Int): Unit = {}

   def g(): Int => Unit =
     if (0 == 0) f else g()
 }
