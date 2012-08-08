//  SI-6157 - Compiler crash on inlined function and -optimize option

object Test {
  def main(args: Array[String]) {
    Console.println(
      ErrorHandler.defaultIfIOException("String")("String")
    )
  }
}

import java.io.IOException

object ErrorHandler {

  @inline
  def defaultIfIOException[T](default: => T)(closure: => T): T = {
    try {
      closure
    } catch {
      case e: IOException =>
        default
    }
  }
}

