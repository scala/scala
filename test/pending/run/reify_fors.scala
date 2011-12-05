import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  val code = scala.reflect.Code.lift{
    object Persons {
      /** A list of persons. To create a list, we use Predef.List
       *  which takes a variable number of arguments and constructs
       *  a list out of them.
       */
      val persons = List(
        new Person("Bob", 17),
        new Person("John", 40),
        new Person("Richard", 68)
      )

      /** A Person class. 'val' constructor parameters become
       *  public members of the class.
       */
      class Person(val name: String, val age: Int)

      /** Return an iterator over persons that are older than 20.
       */
      def olderThan20(xs: Seq[Person]): Iterator[String] =
        olderThan20(xs.elements)

      /** Return an iterator over persons older than 20, given
       *  an iterator over persons.
       */
      def olderThan20(xs: Iterator[Person]): Iterator[String] = {

        // The first expression is called a 'generator' and makes
        // 'p' take values from 'xs'. The second expression is
        // called a 'filter' and it is a boolean expression which
        // selects only persons older than 20. There can be more than
        // one generator and filter. The 'yield' expression is evaluated
        // for each 'p' which satisfies the filters and used to assemble
        // the resulting iterator
        for (p <- xs if p.age > 20) yield p.name
      }
    }

    /** Some functions over lists of numbers which demonstrate
     *  the use of for comprehensions.
     */
    object Numeric {

      /** Return the divisors of n. */
      def divisors(n: Int): List[Int] =
        for (i <- List.range(1, n+1) if n % i == 0) yield i

      /** Is 'n' a prime number? */
      def isPrime(n: Int) = divisors(n).length == 2

      /** Return pairs of numbers whose sum is prime. */
      def findNums(n: Int): Iterable[(Int, Int)] = {

        // a for comprehension using two generators
        for (i <- 1 until n;
             j <- 1 until (i-1);
             if isPrime(i + j)) yield (i, j)
      }

      /** Return the sum of the elements of 'xs'. */
      def sum(xs: List[Double]): Double =
        xs.foldLeft(0.0) { (x, y) => x + y }

      /** Return the sum of pairwise product of the two lists. */
      def scalProd(xs: List[Double], ys: List[Double]) =
        sum(for((x, y) <- xs zip ys) yield x * y);

      /** Remove duplicate elements in 'xs'. */
      def removeDuplicates[A](xs: List[A]): List[A] =
        if (xs.isEmpty)
          xs
        else
          xs.head :: removeDuplicates(for (x <- xs.tail if x != xs.head) yield x)
    }

    // import all members of object 'persons' in the current scope
    import Persons._

    print("Persons over 20:")
    olderThan20(persons) foreach { x => print(" " + x) }
    println

    import Numeric._

    println("divisors(34) = " + divisors(34))

    print("findNums(15) =")
    findNums(15) foreach { x => print(" " + x) }
    println

    val xs = List(3.5, 5.0, 4.5)
    println("average(" + xs + ") = " + sum(xs) / xs.length)

    val ys = List(2.0, 1.0, 3.0)
    println("scalProd(" + xs + ", " + ys +") = " + scalProd(xs, ys))
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(code.tree)
  toolbox.runExpr(ttree)
}
