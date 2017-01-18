object Test extends App {
  println("Hi there Bob".splitLeft(1, ' '))
  println(":load test.scala bogus.scala helloWorld.scala".tail.splitLeft(1, ' '))
  println("zxcxvxbx".splitLeft(2, 'x'))
  println("zxcxvxbx".splitRight(2, 'x'))
}
