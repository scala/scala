package p {
  trait T[X] { def O : { def apply(): X } }
  object `package` extends T[Int] {
    def O: { def apply(): Int } = new { def apply(): Int = 42 }
  }

  object Test {
    def main(args: Array[String]): Unit = {
      val x: Int = O()
    }
  }
}
