package scala.runtime

import org.junit.Test

class RichFloatTest {

  @Test def exponentiationOperator(): Unit = {
    val byte: Byte = 2
    val char: Char = 3
    val short: Short = 3

    assert( 2f\byte == 4 )
    assert( (2f\byte).isInstanceOf[Float] )

    assert( 2f\char == 8 )
    assert( (2f\char).isInstanceOf[Float] )

    assert( 2f\short == 8 )
    assert( (2f\short).isInstanceOf[Float] )

    assert( 2f\0 == 1 )
    assert( (2f\0).isInstanceOf[Float] )

    assert( 2f\1L == 2 )
    assert( (2f\1L).isInstanceOf[Float] )

    assert( 2f\4f == 16 )
    assert( (2f\4f).isInstanceOf[Float] )

    assert( 2f\5d == 32 )
    assert( (2f\5d).isInstanceOf[Double] )

  }

}
