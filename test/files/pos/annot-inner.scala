object test {
   class annot extends scala.annotation.Annotation

   def foo {
     @annot def bar(i: Int): Int = i
     @annot class Silly { }
     bar(5)
   }
}
