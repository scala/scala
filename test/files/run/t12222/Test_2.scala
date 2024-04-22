object Test {
  def main(args: Array[String]): Unit = {
    val vertices = Array[Float]()
    val attribute = new Float32Buffer(vertices)
    assert(attribute.count == 0)
  }
}
