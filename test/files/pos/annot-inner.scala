object test {
   class annot extends scala.annotation.Annotation

   def foo: Unit = {
     @annot def bar(i: Int): Int = i
     @annot class Silly { }
     bar(5)
   }
}
