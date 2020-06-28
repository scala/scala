package scala.runtime

import org.junit.Test

class RichCharTest {

  @Test def exponentiationOperator(): Unit = {
    val char: Char = 2
    val byte: Byte = 2
    val short: Short = 3

    assert( char\byte == 4 )
    assert( (char\byte).isInstanceOf[Int] )

    assert( char\char == 4 )
    assert( (char\char).isInstanceOf[Int] )

    assert( char\short == 8 )
    assert( (char\short).isInstanceOf[Int] )

    assert( char\0 == 1 )
    assert( (char\0).isInstanceOf[Int] )

    assert( char\1L == 2 )
    assert( (char\1L).isInstanceOf[Long] )

    assert( char\4f == 16 )
    assert( (char\4f).isInstanceOf[Float] )

    assert( char\5d == 32 )
    assert( (char\5d).isInstanceOf[Double] )

  }

}
