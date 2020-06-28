package scala.runtime

import org.junit.Test

class RichByteTest {

  @Test def exponentiationOperator(): Unit = {
    val char: Char = 3
    val byte: Byte = 2
    val short: Short = 3

    assert( byte\byte == 4 )
    assert( (byte\byte).isInstanceOf[Int] )

    assert( byte\char == 8 )
    assert( (byte\char).isInstanceOf[Int] )

    assert( byte\short == 8 )
    assert( (byte\short).isInstanceOf[Int] )

    assert( byte\0 == 1 )
    assert( (byte\0).isInstanceOf[Int] )

    assert( byte\1L == 2 )
    assert( (byte\1L).isInstanceOf[Long] )

    assert( byte\4f == 16 )
    assert( (byte\4f).isInstanceOf[Float] )

    assert( byte\5d == 32 )
    assert( (byte\5d).isInstanceOf[Double] )

  }

}
