package scala.runtime

import org.junit.Test

class RichIntTest {

  @Test def exponentiationOperator(): Unit = {
    val byte: Byte = 2
    val char: Char = 3
    val short: Short = 3

    assert( 2\byte == 4 )
    assert( (2\byte).isInstanceOf[Int] )

    assert( 2\char == 8 )
    assert( (2\char).isInstanceOf[Int] )

    assert( 2\short == 8 )
    assert( (2\short).isInstanceOf[Int] )

    assert( 2\0 == 1 )
    assert( (2\0).isInstanceOf[Int] )

    assert( 2\1L == 2 )
    assert( (2\1L).isInstanceOf[Long] )

    assert( 2\4f == 16 )
    assert( (2\4f).isInstanceOf[Float] )

    assert( 2\5d == 32 )
    assert( (2\5d).isInstanceOf[Double] )

  }

}
