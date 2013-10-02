object Test {

  def test() = {
    java.util.Arrays.asList(Array(1,2,3):_*)
  }

  def main(args: Array[String]) {
    println(test())
  }

}
