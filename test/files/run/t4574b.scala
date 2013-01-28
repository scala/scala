object Test extends App {
  class Filter1(ord: Int) {
    def withFilter(f: Any => Boolean) = {
      println(s"${getClass.getSimpleName}#withFilter($ord)")
      this
    }

    def foreach(f: Any => Any) = ()
  }

  for (x <- new Filter1(0)) {} // no filter

  for (0 <- new Filter1(1)) {} // filter

  class Filter2(ord: Int) {
    def filter(f: Any => Boolean) = {
      println(s"${getClass.getSimpleName}#filter($ord)")
      this
    }

    def foreach(f: Any => Any) = ()
  }

  for (x <- new Filter2(2)) {} // no filter

  for (0 <- new Filter2(3)) {} // filter

  implicit val s: String = ""
  class Filter3(ord: Int) {
    def withFilter(f: Any => Boolean)(implicit s: String) = {
      println(s"${getClass.getSimpleName}#withFilter($ord)")
      this
    }

    def foreach(f: Any => Any) = ()
  }

  for (x <- new Filter3(4)) {} // no filter

  for (0 <- new Filter3(5)) {} // filter

  class Filter4(ord: Int) {
    def withFilter[U](f: (Any) => Boolean) = {
      println(s"${getClass.getSimpleName}#withFilter($ord)")
      this
    }

    def foreach(f: Any => Any) = ()
  }

  for (x <- new Filter4(6)) {} // no filter

  for (0 <- new Filter4(7)) {} // filter

  class Filter5(ord: Int) {
    def withFilter[U](f: Boolean => Boolean) = {
      println(s"${getClass.getSimpleName}#withFilter($ord)")
      this
    }

    def foreach(f: Any => Any) = ()
  }

  for (x <- new Filter5(8)) {} // no filter

  for (true <- new Filter5(9)) {} // filter

  for ((true | false) <- new Filter5(10)) {} // no filter

  class Filter6[A](ord: Int) {
    def withFilter[U](f: A => Boolean) = {
      println(s"${getClass.getSimpleName}#withFilter($ord)")
      this
    }

    def foreach(f: Any => Any) = ()
  }

  for ((x, y: String) <- new Filter6[(Int, String)](11)) {} // no filter
  for ((x, "") <- new Filter6[(Int, String)](12)) {} // filter


  class Filter7[A](ord: Int) {
    def withFilter[U](f: A => Boolean) = {
      println(s"${getClass.getSimpleName}#withFilter($ord)")
      new {
        def foreach(f: Any => Any) = ()
      }
    }
    def foreach(f: Any => Any) = ()
  }

  for ((x, y) <- new Filter7[(Int, String)](13)) {} // filter required (we can't elide it if it would switch to a different foreach.)
}
