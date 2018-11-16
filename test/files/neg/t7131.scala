import collection.generic.CanBuildFrom

// leaving out the observable part
trait ObservableValue[-T] {
  def value: T
}

object ObservableValue {

  //this works as is
  implicit class SimpleMappable[T](x: ObservableValue[T]) {
    def map[U](f: T => U) = new ObservableValue[U] {
      def value = f(x.value)
    }
  }

  class TraversableMappable[T, Container[X] <: Traversable[X]](x: ObservableValue[Container[T]]) {

    def map[U, That](f: T => U)(implicit bf: CanBuildFrom[Traversable[T], U, That]): ObservableValue[That] = new ObservableValue[That] {
      def value: That = {
        x.value.map(f)
      }
    }

  }

  //for some reason using an implicit class does not work
  implicit def convertToTraversableMappable[T, Container[X] <: Traversable[X]](x: ObservableValue[Container[T]]) =
    new TraversableMappable(x)

  type HasMap[T, That[_]] = {
    def map[U](f: T => U): That[U]
  }

  class NestedMappable[T, Container[X] <: HasMap[X, Container]](x: ObservableValue[Container[T]]) {

    def map[U](f: T => U): ObservableValue[Container[U]] = new ObservableValue[Container[U]] {
      def value: Container[U] = x.value.map(f)
    }
  }

  //for some reason using an implicit class does not work
  implicit def convertToSimpleMappable[T, Container[X] <: ObservableValue.HasMap[X, Container]](x: ObservableValue[Container[T]]) =
    new NestedMappable(x)

}

object Main extends App {

  class TestCase extends ObservableValue[Int] {
    var value: Int = 0
  }

  val x = new TestCase

  val r = x.map(_ + 1)

  println(r.value) //1

  x.value = 42

  println(r.value) //43


  class TestCase1 extends ObservableValue[Option[Int]] {
    var value: Option[Int] = None
  }

  val x1 = new TestCase1

  val r1 = x1 map ((x: Int) => x + 1)

  println(r1.value) //None

  x1.value = Some(3)

  println(r1.value) //Some(4)

  class TestCase2 extends ObservableValue[List[Int]] {
    var value: List[Int] = List()
  }

  val y = new TestCase2

  val q = y map (_ + 1)

  println(q.value) //List()

  y.value = List(3, 4)

  println(q.value) //List(4, 5)

}
