
package p {
  object Dingo { type Foo = String }
  import Dingo._
  //import Dingo.Foo  // works
  trait Bippy  {
    //import Dingo._  // works
    def z: Foo
    z: String
  }
}
