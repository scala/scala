


import java.io._
import collection._



object Test {

  def check(obj: AnyRef) {
    println(obj)

    val bos = new ByteArrayOutputStream()
    val out = new ObjectOutputStream(bos)
    out.writeObject(obj)
    val arr = bos.toByteArray()
    val in = new ObjectInputStream(new ByteArrayInputStream(arr))
    val deser = in.readObject()

    println(deser)
  }

  def main(args: Array[String]) {
    val lhm = mutable.LinkedHashMap("a" -> "a", "b" -> "b", "c" -> "c")
    val lhs = mutable.LinkedHashSet("a", "b", "c", "d", "e")
    check(lhm)
    check(lhs)
  }

}
