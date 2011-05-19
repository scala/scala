/** Test the Scala implementation of class <code>scala.StringBuilder</code>.
 *
 *  @author Stephane Micheloud
 */
object Test {
  def main(args: Array[String]) {
    Test1.run() //ctor, reverse
    Test2.run() //append
    Test3.run() //insert
    Test4.run()  //indexOf, lastIndexOf
  }
}

object Test1 {
  def run() {
    val j0 = new java.lang.StringBuilder("abc")
    val s0 = new StringBuilder("abc")
    println("s0 equals j0 = " + (s0 equals j0))
    println("s0.toString equals j0.toString = " + (s0.toString equals j0.toString))

    val str = """
Scala is a general purpose programming language designed to express common programming patterns in a concise, elegant, and type-safe way. It smoothly integrates features of object-oriented and functional languages. It is also fully interoperable with Java."""
    val j1 = new java.lang.StringBuilder(100) append str
    val s1 = new java.lang.StringBuilder(100) append str
    println("s1.toString equals j1.toString = " + (s1.toString equals j1.toString))

    val j2 = j0 reverse
    val s2 = s0 reverse;
    println("j2="+j2+", s2="+s2)
    println("s2.toString equals j2.toString = " + (s2.toString equals j2.toString))

    val j3 = j2; j3 setCharAt (0, j3 charAt 2)
    val s3 = s2; s3(0) = s3(2)
    println("j3="+j3+", s3="+s3)
    println("s3.toString equals j3.toString = " + (s3.toString equals j3.toString))
 }
}

object Test2 {
  def run() {
    val j0 = new java.lang.StringBuilder("abc")
    val s0 = new StringBuilder("abc")
    j0 append true append (1.toByte) append 'a' append 9 append -1L append 1.2e-10f append -2.1e+100d
    s0 append true append (1.toByte) append 'a' append 9 append -1L append 1.2e-10f append -2.1e+100d
    println("s0.toString equals j0.toString = " + (s0.toString equals j0.toString))

    val j1 = new java.lang.StringBuilder
    val s1 = new StringBuilder
    j1 append "###" append Array('0', '1', '2') append "xyz".subSequence(0, 3)
    s1 append "###" appendAll Array('0', '1', '2') appendAll List('x', 'y', 'z')
    println("s1.toString equals j1.toString = "  + (s1.toString equals j1.toString))
 }
}

object Test3 {
  def run() {
    val j0 = new java.lang.StringBuilder("abc")
    val s0 = new StringBuilder("abc")
    j0 insert (0, true) insert (0, 1.toByte) insert (0, 'a') insert (0, 88.toShort) insert (0, 9) insert (0, -1L)
    s0 insert (0, true) insert (0, 1.toByte) insert (0, 'a') insert (0, 88.toShort) insert (0, 9) insert (0, -1L)
    println("j0="+j0+", s0="+s0)
    println("s0.toString equals j0.toString = " + (s0.toString equals j0.toString))

    val j1 = new java.lang.StringBuilder
    val s1 = new StringBuilder
    j1 insert (0, "###") insert (0, Array('0', '1', '2')) insert (0, "xyz".subSequence(0, 3))
    s1 insert (0, "###") insertAll (0, Array('0', '1', '2')) insertAll (0, List('x', 'y', 'z'))
    println("j1="+j1+", s1="+s1)
    println("s1.toString equals j1.toString = " + (s1.toString equals j1.toString))
  }
}

object Test4 {
  def run() {
    val j0 = new java.lang.StringBuilder("abc") // Java 1.5+
    val s0 = new StringBuilder("abc")

    val j1 = j0 indexOf("c")
    val s1 = s0 indexOf("c")
    println("j1="+j1+", s1="+s1)
    println("s1 == j1 = " + (s1 == j1))

    val j2 = j0 append "123abc" lastIndexOf("c")
    val s2 = s0 append "123abc" lastIndexOf("c")
    println("j2="+j2+", s2="+s2)
    println("s2 == j2 = " + (s2 == j2))
 }
}
