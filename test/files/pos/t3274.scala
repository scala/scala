trait A { this: B => 
  trait X { 
    class Y1 extends Y
  } 
}

trait B extends A {
  trait Y { def f {} }
}