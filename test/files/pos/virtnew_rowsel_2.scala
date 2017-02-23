object Test extends App {
  trait Rep[T]
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")

  class MyStruct extends Struct // Predef.Struct
  class ApplyDynamicOps {
    def selectDynamic[T](n: String): Rep[T] = error(n)
  }
  implicit def applyDynamicOps[T <: MyStruct](qual: Rep[T]): ApplyDynamicOps = new ApplyDynamicOps

  val qual = new MyStruct{ val xxx: Rep[Int] = null }
  val x: Rep[Int] = qual.xxx // becomes `applyDynamicOps[MyStruct{val xxx: Int}](qual).applyDynamic[Int]("xxx")()`
}
