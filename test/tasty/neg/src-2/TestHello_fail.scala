package helloworld

object TestHello {
  HelloWorld.higherBounded2(List(""))
  HelloWorld.higherBounded3(List(List("")))
  HelloWorld.higherBounded4(Left(""))

  trait Show[-A]

  HelloWorld.higherBounded5[Show]
  HelloWorld.higherBounded6[List]
}
