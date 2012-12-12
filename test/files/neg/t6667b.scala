object Test {
  abstract class Box {
    val value: Int
  }

  implicit val a: Box = new Box {
    val value= 1
  }

  def main(args: Array[String]) {
    implicit val b: Box= new Box {
      val value= 2
    }

    new Object {
      new Test()
    }
    // compare with:
    new Test()
  }
}

class Test()(implicit x: Test.Box) {
  println(x.value)
}
