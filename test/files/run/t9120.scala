import scala.language.postfixOps

object Test {
  import scala.util.control.BreakableGenerators._

  def output[A](expr: => TraversableOnce[A]) {
    val r = expr
    println(r.mkString(" "))
  }

  def main(args: Array[String]) {
    // breakable generator
    output {
      for {
        loop <- breakable(Iterator.from(1));
        j <- loop;
        if { j > 3 } break(loop)
      } yield { j }
    }

    // break with continue
    output {
      for {
        loop <- breakable(Iterator.from(1));
        j <- loop;
        if { j % 2 == 0 } continue;
        if { j > 10 } break(loop)
      } yield { j }
    }

    // test continue
    output {
      for {
        loop <- breakable(1 to 10);
        j <- loop;
        if { j % 2 == 1 } continue
      } yield { j }
    }

    // nested breakable generators
    output {
      for {
        outer <- breakable(1 to 7);
        j <- outer;
        if { j % 2 == 0 } continue;
        inner <- breakable(List("a", "b", "c", "d", "e"));
        k <- inner;
        if { k == "d" } break(inner);
        if { j == 5  &&  k == "c" } break(inner, outer)
      } yield {
        (j, k)
      }
    }

    // infinite breakable generator
    output {
      var x = 1
      for {
        loop <- infinite;
        _ <- loop;
        sq = x * x;
        if { sq > 20 } break(loop);
        _ = { x += 1 }
      } yield {
        sq
      }
    }
  }
}
