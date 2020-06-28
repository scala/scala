package scala.runtime

import org.junit.Test

class RichShortTest {

  @Test def exponentiationOperator(): Unit = {
    val byte: Byte = 2
    val char: Char = 2
    val short: Short = 2

    assert( short\byte == 4 )
    assert( (short\byte).isInstanceOf[Int] )

    assert( short\char == 4 )
    assert( (short\char).isInstanceOf[Int] )

    assert( short\short == 4 )
    assert( (short\short).isInstanceOf[Int] )

    assert( short\0 == 1 )
    assert( (short\0).isInstanceOf[Int] )

    assert( short\1L == 2 )
    assert( (short\1L).isInstanceOf[Long] )

    assert( short\4f == 16 )
    assert( (short\4f).isInstanceOf[Float] )

    assert( short\5d == 32 )
    assert( (short\5d).isInstanceOf[Double] )

  }

}
