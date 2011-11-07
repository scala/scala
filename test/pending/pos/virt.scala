object Virt extends Application {
  class Foo { 
    trait Inner <: { val x : Int = 3 }
  }

  class Bar extends Foo { 
    trait Inner <: { val y : Int = x }
  }
}
