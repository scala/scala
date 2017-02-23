object Test extends App {
  trait Rep[T]
  class MyStruct extends Struct
  class MyQual extends Rep[MyStruct{val xxx: Int}] {
    def selectDynamic[T](n: String): Rep[T] = error(n)
  }
  val qual = new MyQual
  val x: Rep[Int] = qual.xxx // becomes `qual.applyDynamic[Int]("xxx")()`
}
