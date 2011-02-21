import scala.collection.mutable.{Buffer, ArrayBuffer}

class RichBuffer[T, B[U] <: Buffer[U]](buffer: Buffer[T]) {
  def mymap[S](f: T => S)(implicit rv: B[S]): B[S] = {
    buffer.foreach{ e =>
      rv += f(e)
    }
    rv
  }
}

object App {
  def mymap2[T, B[U] <: Buffer[U], S](buffer: B[T], f: T => S)(implicit rv: B[S]): B[S] = {
    buffer.foreach{ e =>
      rv += f(e)
    }
    rv
  }

  def mymap3[T, B <: Buffer[T], S](buffer: B, f: T => T)(implicit rv: B): B = {
    buffer.foreach{ e =>
      rv += f(e)
    }
    rv
  }

  def mymap4[T, B[U] <: Buffer[U], S](buffer: B[T])(f: T => S) (implicit rv: B[S]): B[S] = {
    buffer.foreach{ e =>
      rv += f(e)
    }
    rv
  }


  def main(args: Array[String]) {
    implicit def richBuffer[T, B[U] <: Buffer[U]](buffer: B[T]): RichBuffer[T, B] =
      new RichBuffer[T, B](buffer)

    implicit val rv = new ArrayBuffer[Int]
    val buf = new ArrayBuffer[Int]
    (1 to 5).foreach(buf += _)
    buf.mymap(x => x*x)
    richBuffer(buf).mymap[Int](x => x*x)
    richBuffer[Int, ArrayBuffer](buf).mymap[Int](x => x*x)
    mymap2(buf, (x: Int) => x*x)
    mymap2[Int, ArrayBuffer, Int](buf, (x: Int) => x*x)
    // mymap3(buf, x => x*x)                                     // compiler error
    mymap3(buf, (x: Int) => x*x)
    mymap4(buf)(x => x*x)
  }
}
