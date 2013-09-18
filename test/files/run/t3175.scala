/** A bit down the road this test will examine
 *  the bytecode.
 */

import scala.language.reflectiveCalls

object Test {
  def len(x:{ def length: Int }) = x.length
  def f1(x:{ def apply(x: Int): Long }) = x(0)
  def f2(x:{ def apply(x: Int): Byte }) = x(0)
  def f3(x:{ def apply(x: Int): String }) = x(0).length

  def f4(x:{ def update(x: Int, y: Long): Unit }, y: Long) = x(0) = y
  def f5(x:{ def update(x: Int, y: Byte): Unit }, y: Byte) = x(0) = y
  def f6(x:{ def update(x: Int, y: String): Unit }, y: String) = x(0) = y

  def f7(x: { def length: Any }) = x.length

  def f8(x: { def apply(x: Int): Any }) = x(0)
  def f9(x: { def apply(x: Int): Int }) = x(0)
  def f10(x: { def apply(x: Int): Long }) = x(0)

  // update has some interesting special cases
  def f11(x:{ def update(x: Int, y: Long): Any }, y: Long) = x(0) = y
  def f12(x:{ def update(x: Int, y: String): AnyVal }, y: String) = x(0) = y
  def f13(x:{ def update(x: Int, y: String): AnyRef }, y: String) = x(0) = y

  // doesn't work yet, see #3197
  // def fclone(x:{ def clone(): AnyRef }) = x.clone()

  def main(args: Array[String]): Unit = {
    val longs = Array(5L)
    val bytes = Array(5: Byte)
    val strs = Array("abcde", "fghjij")

    println(len(Array(1,2,3)) + len(Array(4.0,5.0f)) + len(Array("abc", 5)) + len("bop"))
    println(f1(longs) + f2(bytes) + f3(strs))

    f4(longs, 1)
    f5(bytes, 1)
    f6(strs, "a")

    println(f1(longs) + f2(bytes) + f3(strs))

    println(f7(Array(1,2,3)))
    println(f7("def"))

    println(f8(Array(5)))
    println(f9(Array(5)))
    println(f10(Array(5)))

    f11(longs, 100L)
    f12(strs, "jabooboo")
    println(longs(0))
    println(strs(0))
    f13(new { def update(x: Int, y: String): List[Int] = { println("hi mom") ; Nil } }, "irrelevant")
  }
}
