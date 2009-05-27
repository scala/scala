package examples

import scala.xml._


object fors {

  val e = Node.NoAttributes

  class Person(_name: String, _age: Int) {
    val name = _name
    val age = _age
  }

  def printOlderThan20(xs: Seq[Person]): Iterator[String] =
    printOlderThan20(xs.iterator)

  def printOlderThan20(xs: Iterator[Person]): Iterator[String] =
    for (p <- xs if p.age > 20) yield p.name

  val persons = List(
    new Person("John", 40),
    new Person("Richard", 68)
  )

  def divisors(n: Int): List[Int] =
    for (i <- List.range(1, n+1) if n % i == 0) yield i

  def isPrime(n: Int) = divisors(n).length == 2

  def findNums(n: Int): Iterable[(Int, Int)] =
    for (i <- 1 until n;
         j <- 1 until (i-1);
         if isPrime(i+j)) yield (i, j)

  def sum(xs: List[Double]): Double =
    xs.foldLeft(0.0) { (x, y) => x + y }

  def scalProd(xs: List[Double], ys: List[Double]) =
    sum(for((x, y) <- xs zip ys) yield x * y)

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
    for (Elem(_, "book", _, _, book @ _*) <- books;
         Elem(_, "title", _, _, Text(title)) <- book.toList;
         if (title indexOf "Program") >= 0;
         Elem(_, "author", _, _, Text(author)) <- List(book)) yield author

  for (Elem(_, "book", _, _, book @ _*) <- books;
       Elem(_, "author", _, _, Text(author)) <- book.toList;
       if author startsWith "Ullman";
       Elem(_, "title", _, _, Text(title)) <- List(book)) yield title

  removeDuplicates(
    for (Elem(_, "book", _, _, b1 @ _* ) <- books;
         Elem(_, "book", _, _, b2 @ _*) <- books;
         if b1 != b2;
         Elem(_, "author", _, _, Text(a1)) <- b1.toList;
         Elem(_, "author", _, _, Text(a2)) <- b2.toList;
         if a1 == a2) yield Pair(a1, a2))

  def removeDuplicates[a](xs: List[a]): List[a] =
    if (xs.isEmpty)
      xs
    else
      xs.head :: removeDuplicates(for (x <- xs.tail if x != xs.head) yield x)

  def main(args: Array[String]) {
    print("Persons over 20:")
    printOlderThan20(persons) foreach { x => print(" " + x) }
    println

    println("divisors(34) = " + divisors(34))

    print("findNums(15) =");
    findNums(15) foreach { x => print(" " + x); }
    println

    val xs = List(3.5, 5.0, 4.5)
    println("average(" + xs + ") = " + sum(xs) / xs.length)

    val ys = List(2.0, 1.0, 3.0)
    println("scalProd(" + xs + ", " + ys +") = " + scalProd(xs, ys))
  }

}
