object A {
   def x = 3

   def y = {
     import B._
     x
   }
}
