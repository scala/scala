final class NonNullChar(val get: Char) extends AnyVal {
  def isEmpty = get == 0.toChar
  override def toString = if (isEmpty) "NoChar" else s"'$get'"
}
object NonNullChar {
  @inline final val None = new NonNullChar(0.toChar)
}

final class SomeProduct extends Product3[String, Int, List[String]] {
  def canEqual(x: Any) = x.isInstanceOf[SomeProduct]
  def _1 = "abc"
  def _2 = 5
  def _3 = List("bippy")
  def isEmpty = false
  def get = this
}
object SomeProduct {
  def unapply(x: SomeProduct) = x
}

object Test {
  def prod(x: SomeProduct): Int = x match {
    case SomeProduct(x, y, z) => x.length + y + z.length
    case _                    => -1
  }

  def f(x: Char): NonNullChar = x match {
    case 'a' => new NonNullChar('a')
    case 'b' => new NonNullChar('b')
    case 'c' => new NonNullChar('c')
    case _   => NonNullChar.None
  }
  // public char f(char);
  //        0: iload_1
  //        1: tableswitch   { // 97 to 99
  //                     97: 47
  //                     98: 42
  //                     99: 37
  //                default: 28
  //           }
  //       28: getstatic     #19                 // Field NonNullChar$.MODULE$:LNonNullChar$;
  //       31: invokevirtual #23                 // Method NonNullChar$.None:()C
  //       34: goto          49
  //       37: bipush        99
  //       39: goto          49
  //       42: bipush        98
  //       44: goto          49
  //       47: bipush        97
  //       49: ireturn
  def g(x: Char): Option[Char] = x match {
    case 'a' => Some('a')
    case 'b' => Some('b')
    case 'c' => Some('c')
    case _   => None
  }
  // public scala.Option<java.lang.Object> g(char);
  //        0: iload_1
  //        1: tableswitch   { // 97 to 99
  //                     97: 64
  //                     98: 49
  //                     99: 34
  //                default: 28
  //           }
  //       28: getstatic     #33                 // Field scala/None$.MODULE$:Lscala/None$;
  //       31: goto          76
  //       34: new           #35                 // class scala/Some
  //       37: dup
  //       38: bipush        99
  //       40: invokestatic  #41                 // Method scala/runtime/BoxesRunTime.boxToCharacter:(C)Ljava/lang/Character;
  //       43: invokespecial #44                 // Method scala/Some."<init>":(Ljava/lang/Object;)V
  //       46: goto          76
  //       49: new           #35                 // class scala/Some
  //       52: dup
  //       53: bipush        98
  //       55: invokestatic  #41                 // Method scala/runtime/BoxesRunTime.boxToCharacter:(C)Ljava/lang/Character;
  //       58: invokespecial #44                 // Method scala/Some."<init>":(Ljava/lang/Object;)V
  //       61: goto          76
  //       64: new           #35                 // class scala/Some
  //       67: dup
  //       68: bipush        97
  //       70: invokestatic  #41                 // Method scala/runtime/BoxesRunTime.boxToCharacter:(C)Ljava/lang/Character;
  //       73: invokespecial #44                 // Method scala/Some."<init>":(Ljava/lang/Object;)V
  //       76: areturn
  def main(args: Array[String]): Unit = {
    "abcd" foreach (ch => println(f(ch)))
    "abcd" foreach (ch => println(g(ch)))
    println(prod(new SomeProduct))
  }
}


