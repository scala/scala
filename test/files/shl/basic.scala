//############################################################################
// Basic Operations
//############################################################################
// $Id$

()
false
'1'
2
3l
4.0f
5.0d
null
"hello"
Nil
List(1,2,3)

throw new Error("error")

val x = 5;
x
y

def fact(n: Int): Int = if (n == 1) 1 else n*fact(n - 1)
fac(5)
fact(5)

def length[X](a: Int, ls: List[X]): Int =
  if (ls.isEmpty) a else length(a + 1, ls.tail)
length(Nil)
length(0, Nil)
length(0, List(1,2,3,4,5))

class Point(x: Int, y: Int) {
  def move(dx: Int, dy: Int): Point = new Point(x + dx, y + dy);
  override def toString(): String = "Point(" + x + "," + y + ")";
}
val p1 = new Point(3,7)
p1.move(7,3)

"string".isInstanceOf[String]
"string".isInstanceOf[Int]
"string".asInstanceOf[String]
"string".asInstanceOf[Int]

//############################################################################
