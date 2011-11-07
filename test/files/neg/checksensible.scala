class Test {
  var c = 0
  println((c = 1) == 0)

  println(1 == "abc")
  println(1 != true)

  println(((x: Int) => x + 1) == null)
  println(new Object == new Object)
  println(new Exception() != new Exception())

  val foo: Array[String] = Array("1","2","3")
  if (foo.length == null) //  == 0 makes more sense, but still
    println("plante")     // this code leads to runtime crash
  else
    println("plante pas")

  def main(args: Array[String]) = {
    val in = new java.io.FileInputStream(args(0))

    var c = 0    
    while ((c = in.read) != -1)
      print(c.toChar)

    in.close
  }

  println({} == true)
  println("hello" == 2)
  println(new Object == 1)
  println(1 == (new Object))

  def isabstract: Int

  println(1 != println)
  println(1 != 'sym)

}
