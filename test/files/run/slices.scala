object Test extends App {

  // lists
  println(List(1, 2, 3, 4).slice(1, 2))
  println(List(1, 2, 3, 4).slice(2, 1))
  println(List(1, 2, 3, 4).slice(-1, 1))
  println(List(1, 2, 3, 4).slice(1, -1))
  println(List(1, 2, 3, 4).slice(-2, 2))
  println

  println(List(1, 2, 3, 4) take 3)
  println(List(1, 2, 3) take 3)
  println(List(1, 2) take 3)
  println((List(): List[Int]) take 3)
  println(List[Nothing]() take 3)
  println

  println(List(1, 2, 3, 4) drop 3)
  println(List(1, 2, 3) drop 3)
  println(List(1, 2) drop 3)
  println((List(): List[Int]) drop 3)
  println(List[Nothing]() drop 3)
  println

  // arrays
  println(Array(1, 2, 3, 4).slice(1, 2).deep)
  println(Array(1, 2, 3, 4).slice(2, 1).deep)
  println(Array(1, 2, 3, 4).slice(-1, 1).deep)
  println(Array(1, 2, 3, 4).slice(1, -1).deep)
  println(Array(1, 2, 3, 4).slice(-2, 2).deep)
  println

  println(Array(1, 2, 3, 4) take 3 deep)
  println(Array(1, 2, 3) take 3 deep)
  println(Array(1, 2) take 3 deep)
  println((Array(): Array[Int]) take 3 deep)
//  println(Array[Nothing]() take 3)  // contrib #757
  println

  println(Array(1, 2, 3, 4) drop 3 deep)
  println(Array(1, 2, 3) drop 3 deep)
  println(Array(1, 2) drop 3 deep)
  println((Array(): Array[Int]) drop 3 deep)
//  println(Array[Nothing]() drop 3)
  println
}
