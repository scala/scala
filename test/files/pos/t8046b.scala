trait One {
  type Op[A]
  type Alias = Op[Int]
}
 
trait Two extends One {
  trait Op[A] extends M[A]
  //(a: Alias) => a.value.toChar               // okay
                                             // (=> A).asSeenFrom(a.type, trait M): => Int
  class View2 extends Alias { value.toChar } // toChar is not a member of type parameter A
                                             // (=> A).asSeenFrom(View2.this.type, trait M): => A
  
  // override type Alias = Op[Int]           // works with this
}

trait M[A] { def value: A = sys.error("") }
