object test {
   class annot extends Annotation

   def foo {
     @annot def bar(i: Int): Int = i
     @annot class Silly { }
     bar(5)
   }
}
