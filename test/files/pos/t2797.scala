class MyVector[A] {
  def map[B](f: A => B): MyVector[B] = sys.error("")
}

object Test {
  def unzip[B, C](_this: MyVector[(B, C)]): (MyVector[B], MyVector[C]) = {
    (_this.map{ bc => bc._1 }, _this.map{ bc => bc._2 })
  }
}
