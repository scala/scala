object Traits {
  trait OuterClass[V <: OuterClass[V]#InnerClass] {
  trait InnerClass {self: V =>
      def method = ()
    }
  }

  trait SubOuterClass[T <: SubOuterClass[T]#SubInnerClass] extends OuterClass[T] {
    trait SubInnerClass extends super.InnerClass {self: T =>  }
  }

  trait SubOuterClass2[T <: SubOuterClass2[T]#SubInnerClass2] extends OuterClass[T] {
    trait SubInnerClass2 extends super.InnerClass {self: InnerClass with T =>  }
  }

}

// object Classes {
//   class OuterClass[V <: OuterClass[V]#InnerClass] {
//   class InnerClass {self: V =>
//       def method = ()
//     }
//   }

//   class SubOuterClass[T <: SubOuterClass[T]#SubInnerClass] extends OuterClass[T] {
//     class SubInnerClass extends super.InnerClass {self: T =>  }
//   }

//   class SubOuterClass2[T <: SubOuterClass2[T]#SubInnerClass2] extends OuterClass[T] {
//     class SubInnerClass2 extends super.InnerClass {self: InnerClass with T =>  }
//   }
// }
