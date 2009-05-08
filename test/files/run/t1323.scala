object Test extends Application {
  println(" 1:" + List(1,2,3,4).indexOfSeq(List(0,1)))        // -1
  println(" 2:" + List(1,2,3,4).indexOfSeq(List(1,2)))        //  0
  println(" 3:" + List(1,2,3,4).indexOfSeq(List(2,3)))        //  1
  println(" 4:" + List(1,2,3,4).indexOfSeq(List(3,4)))        //  2
  println(" 5:" + List(1,2,3,4).indexOfSeq(List(4,5)))        // -1
  println(" 6:" + List(1,2,3,4).indexOfSeq(List(2,4)))        // -1
  println(" 7:" + List(1,2,3,4).indexOfSeq(List(4,3)))        // -1
  println(" 8:" + List(1,2,3,4).indexOfSeq(List(1,3)))        // -1
  println(" 9:" + List(1,2,3,4).indexOfSeq(List(1,3)))        // -1
  println("10:" + List(1,2,3,4).indexOfSeq(List(1,2,3,4)))    //  0
  println("11:" + List(1,2,3,4).indexOfSeq(List(4,3,2,1)))    // -1
  println("12:" + List(1,2,3,4).indexOfSeq(List(1,2,3,4,5)))  // -1
  println("13:" + List(1,2,3,4).indexOfSeq(List(5,4,3,2,1)))  // -1
  println("14:" + List(1,2,3,4).indexOfSeq(List()))           //  0
  println("15:" + List().indexOfSeq(List()))                  //  0
  println("16:" + List().indexOfSeq(List(1,2,3,4)))           // -1

  // Do some testing with infinite sequences
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  println("17:" + List(1,2,3,4).indexOfSeq(from(1)))          // -1
  println("18:" + from(1).indexOfSeq(List(4,5,6)))            // 3
}

