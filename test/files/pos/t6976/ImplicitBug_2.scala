trait Support extends Exts

// object ImplicitsBug extends App with Support { // A
object ImplicitsBug extends App with Exts { // B
  //Exts // C) this reference helped in the large project.
  println(3.moo)
}
