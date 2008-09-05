object Comp extends Application {

   trait Family {
     type T
   }

   object Trivial extends Family {
     type T = Unit
   }

   trait Wrap extends Family {
     val v : Family
     type T = v.T
   }

   object WrapTrivial extends Wrap {
     val v = Trivial
   }

   object WrapWrapTrivial extends Wrap {
     val v = WrapTrivial
   }

}
