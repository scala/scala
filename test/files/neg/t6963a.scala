object Test {
  import scala.collection.mutable._

  List(1,2,3,4,5).scanRight(0)(_+_)
}
