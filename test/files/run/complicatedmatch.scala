object Bar{
  def unapply(x : String) = x == "bar";
}

object Even{
  def unapply(x : Int) = if (x % 2 == 0) Some(x / 2) else None;
}

object Test extends App{
  val LongWord = "supercalifragilisticexpialadocious";

  def foo(x : Int, y : String) : Int = (x, y) match {
    case (Even(i), "bar") => 1
    case (1 | 2 | 3, "foo") => 42;
    case (x, y) if y.length < x => 11;
    case (1 | 2 | 3, Bar()) => 7;
    case (1 | 2 | 3, "bar") => 8
    case (Even(Even(3)), Bar()) => 13;
    case (Even(Even(3)), LongWord) => 13;
    case _ => 0;
  }

  List(
    2 -> "bar",
    2 -> "foo",
    3 -> "foo",
    7 -> "flob",
    3 -> "bar",
    12 -> LongWord
  ).foreach({case (x, y) => println(foo(x, y))});
}
