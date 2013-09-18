// This is here to tell me if the behavior changes, not because
// the output is endorsed.
case class Two[
  @specialized(Specializable.Everything) A,
  @specialized(Specializable.Everything) B
](v: A, w: B)

case class TwoLong[
  @specialized(Char, Boolean, Byte, Short, Int, Long, Float, Double, Unit, AnyRef) A,
  @specialized(Char, Boolean, Byte, Short, Int, Long, Float, Double, Unit, AnyRef) B
](v: A, w: B)

case class TwoCool[
  @specialized(Specializable.Everything) A,
  @specialized(Specializable.Everything) B
](v: A, w: B)

case class TwoShort[
  @specialized(Specializable.Everything) A,
  @specialized(Specializable.Everything) B
](v: A, w: B)

case class TwoMinimal[
  @specialized(Int, AnyRef) A,
  @specialized(Int, AnyRef) B
](v: A, w: B)

object Test {
  def main(args: Array[String]): Unit = {
    println(Two("Hello", "World").getClass().getName());
    println(Two(12, "Hello").getClass().getName());
    println(Two("Hello", 12).getClass().getName());
    println(Two(12, 12).getClass().getName());

    println(TwoLong("Hello", "World").getClass().getName());
    println(TwoLong(12, "Hello").getClass().getName());
    println(TwoLong("Hello", 12).getClass().getName());
    println(TwoLong(12, 12).getClass().getName());

    println(TwoCool("Hello", "World").getClass().getName());
    println(TwoCool(12, "Hello").getClass().getName());
    println(TwoCool("Hello", 12).getClass().getName());
    println(TwoCool(12, 12).getClass().getName());

    println(TwoShort("Hello", "World").getClass().getName());
    println(TwoShort(12, "Hello").getClass().getName());
    println(TwoShort("Hello", 12).getClass().getName());
    println(TwoShort(12, 12).getClass().getName());

    println(TwoMinimal("Hello", "World").getClass().getName());
    println(TwoMinimal(12, "Hello").getClass().getName());
    println(TwoMinimal("Hello", 12).getClass().getName());
    println(TwoMinimal(12, 12).getClass().getName());
  }
}
