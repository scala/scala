object fors {

  class Person(_name: String, _age: Int) {
    val name = _name;
    val age = _age;
  }

  def printOlderThan20(xs: Seq[Person]): Iterator[String] =
    printOlderThan20(xs.elements);

  def printOlderThan20(xs: Iterator[Person]): Iterator[String] =
    for (val p <- xs; p.age > 20) yield p.name;

  val persons = List(
    new Person("John", 40),
    new Person("Richard", 68)
  );

  def divisors(n: Int): List[Int] =
    for (val i <- List.range(1, n+1); n % i == 0) yield i;

  def isPrime(n: Int) = divisors(n).length == 2;

  def findNums(n: Int): Iterator[Pair[Int, Int]] =
    for (val i <- Iterator.range(1, n);
         val j <- Iterator.range(1, i-1);
         isPrime(i+j)) yield Pair(i, j);

  def sum(xs: List[Double]): Double =
    xs.foldLeft(0.0) { (x, y) => x + y }

  def scalProd(xs: List[Double], ys: List[Double]) =
    sum(for(val Pair(x, y) <- xs zip ys) yield x * y);

  type Lst = List[Any];

  val books = List(
    'book('title("Structure and Interpretation of Computer Programs"),
          'author("Abelson, Harald"),
          'author("Sussman, Gerald J.")),
    'book('title("Principles of Compiler Design"),
          'author("Aho, Alfred"),
          'author("Ullman, Jeffrey")),
    'book('title("Programming in Modula-2"),
	  'author("Wirth, Niklaus")));

  def findAuthor(books: Lst) =
    for (val 'book(book: Lst) <- books;
         val 'title(title: String) <- book;
         (title indexOf "Program") >= 0;
         val 'author(author: String) <- book) yield author;

  for (val 'book(b: Lst) <- books;
       val 'author(author: String) <- b;
       author startsWith "Ullman";
       val 'title(title: String) <- b) yield title;

  removeDuplicates(
    for (val 'book(b1: Lst) <- books;
       val 'book(b2: Lst) <- books;
	 b1 != b2;
	 val 'author(a1: String) <- b1;
	 val 'author(a2: String) <- b2;
	 a1 == a2) yield Pair(a1, a2));

  def removeDuplicates[a](xs: List[a]): List[a] =
    if (xs.isEmpty)
      xs
    else
      xs.head :: removeDuplicates(for (val x <- xs.tail; x != xs.head) yield x);

  def main(args: Array[String]) = {
    Console.print("Persons over 20:");
    printOlderThan20(persons) foreach { x => Console.print(" " + x) };
    Console.println;

    Console.println("divisors(34) = " + divisors(34));

    Console.print("findNums(15) =");
    findNums(15) foreach { x => Console.print(" " + x); };
    Console.println;

    val xs = List(3.5, 5.2);
    Console.println("average(" + xs + ") = "
      + sum(xs) / xs.length);

    val ys = List(2.0, 1.0);
    Console.println("scalProd(" + xs + ", " + ys +") = "
      + scalProd(xs, ys));
  }

}
