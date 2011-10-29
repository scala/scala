trait OuterClass[V <: OuterClass[V]#InnerClass] {
  trait InnerClass {self: V =>
    def method = ()
  }
}

trait SubOuterClass[T <: SubOuterClass[T]#SubInnerClass] extends OuterClass[T] {
  class SubInnerClass extends super.InnerClass {self: T =>  }
}

trait SubOuterClass2[T <: SubOuterClass2[T]#SubInnerClass2] extends OuterClass[T] {
  class SubInnerClass2 extends super.InnerClass {self: InnerClass with T =>  }
}
