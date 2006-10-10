package examples

import scala.xml._


object fors {

  val e = Node.NoAttributes

  class Person(_name: String, _age: Int) {
    val name = _name
    val age = _age
  }

  def printOlderThan20(xs: Seq[Person]): Iterator[String] =
    printOlderThan20(xs.elements)

  def printOlderThan20(xs: Iterator[Person]): Iterator[String] =
    for (val p <- xs; p.age > 20) yield p.name

  val persons = List(
    new Person("John", 40),
    new Person("Richard", 68)
  )

  def divisors(n: Int): List[Int] =
    for (val i <- List.range(1, n+1); n % i == 0) yield i

  def isPrime(n: Int) = divisors(n).length == 2

  def findNums(n: Int): Iterator[Pair[Int, Int]] =
    for (val i <- Iterator.range(1, n);
         val j <- Iterator.range(1, i-1);
         isPrime(i+j)) yield Pair(i, j)

  def sum(xs: List[Double]): Double =
    xs.foldLeft(0.0) { (x, y) => x + y }

  def scalProd(xs: List[Double], ys: List[Double]) =
    sum(for(val Pair(x, y) <- xs zip ys) yield x * y)

  type Lst = List[Any]

  val prefix = null
  val scope = TopScope

  val books = List(
    Elem(prefix, "book", e, scope,
         Elem(prefix, "title", e, scope,
              Text("Structure and Interpretation of Computer Programs")),
         Elem(prefix, "author", e, scope,
              Text("Abelson, Harald")),
         Elem(prefix, "author", e, scope,
              Text("Sussman, Gerald J."))),
    Elem(prefix, "book", e, scope,
         Elem(prefix, "title", e, scope,
              Text("Principles of Compiler Design")),
         Elem(prefix, "author", e, scope,
              Text("Aho, Alfred")),
         Elem(prefix, "author", e, scope,
              Text("Ullman, Jeffrey"))),
    Elem(prefix, "book", e, scope,
         Elem(prefix, "title", e, scope,
              Text("Programming in Modula-2")),
         Elem(prefix, "author", e, scope,
              Text("Wirth, Niklaus")))
  )

  def findAuthor(books: Lst) =
    for (val Elem(_, "book", _, _, book @ _*) <- books;
         val Elem(_, "title", _, _, Text(title)) <- book.toList;
         (title indexOf "Program") >= 0;
         val Elem(_, "author", _, _, Text(author)) <- List(book)) yield author

  for (val Elem(_, "book", _, _, book @ _*) <- books;
       val Elem(_, "author", _, _, Text(author)) <- book.toList;
       author startsWith "Ullman";
       val Elem(_, "title", _, _, Text(title)) <- List(book)) yield title

  removeDuplicates(
    for (val Elem(_, "book", _, _, b1 @ _* ) <- books;
         val Elem(_, "book", _, _, b2 @ _*) <- books;
         b1 != b2;
         val Elem(_, "author", _, _, Text(a1)) <- b1.toList;
         val Elem(_, "author", _, _, Text(a2)) <- b2.toList;
         a1 == a2) yield Pair(a1, a2))

  def removeDuplicates[a](xs: List[a]): List[a] =
    if (xs.isEmpty)
      xs
    else
      xs.head :: removeDuplicates(for (val x <- xs.tail; x != xs.head) yield x)

  def main(args: Array[String]) = {
    Console.print("Persons over 20:")
    printOlderThan20(persons) foreach { x => Console.print(" " + x) }
    Console.println

    Console.println("divisors(34) = " + divisors(34))

    Console.print("findNums(15) =");
    findNums(15) foreach { x => Console.print(" " + x); }
    Console.println

    val xs = List(3.5, 5.0, 4.5)
    Console.println("average(" + xs + ") = " + sum(xs) / xs.length)

    val ys = List(2.0, 1.0, 3.0)
    Console.println("scalProd(" + xs + ", " + ys +") = " + scalProd(xs, ys))
  }

}
