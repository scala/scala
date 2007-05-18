object C extends Application {
  import HolderA.A
  import HolderB.B

  println(new A(4) with B)
}
