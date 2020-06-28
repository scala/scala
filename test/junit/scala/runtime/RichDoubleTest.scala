package scala.runtime

import org.junit.Test

class RichDoubleTest {

  @Test def exponentiationOperator(): Unit = {
    val byte: Byte = 2
    val char: Char = 2
    val short: Short = 3

    assert( 2d\byte == 4 )
    assert( (2d\byte).isInstanceOf[Double] )

    assert( 2d\char == 4 )
    assert( (2d\char).isInstanceOf[Double] )

    assert( 2d\short == 8 )
    assert( (2d\short).isInstanceOf[Double] )

    assert( 2d\0 == 1 )
    assert( (2d\0).isInstanceOf[Double] )

    assert( 2d\1L == 2 )
    assert( (2d\1L).isInstanceOf[Double] )

    assert( 2d\4f == 16 )
    assert( (2d\4f).isInstanceOf[Double] )

    assert( 2d\5d == 32 )
    assert( (2d\5d).isInstanceOf[Double] )

  }

}
