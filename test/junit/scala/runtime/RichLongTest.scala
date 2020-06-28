package scala.runtime

import org.junit.Test

class RichLongTest {

  @Test def exponentiationOperator(): Unit = {
    val byte: Byte = 2
    val char: Char = 3
    val short: Short = 3

    assert( 2L\byte == 4 )
    assert( (2L\byte).isInstanceOf[Long] )

    assert( 2L\char == 8 )
    assert( (2L\char).isInstanceOf[Long] )

    assert( 2L\short == 8 )
    assert( (2L\short).isInstanceOf[Long] )

    assert( 2L\0 == 1 )
    assert( (2L\0).isInstanceOf[Long] )

    assert( 2L\1L == 2 )
    assert( (2L\1L).isInstanceOf[Long] )

    assert( 2L\4f == 16 )
    assert( (2L\4f).isInstanceOf[Float] )

    assert( 2L\5d == 32 )
    assert( (2L\5d).isInstanceOf[Double] )

  }

}
