//> using jvm 21+
//> using options --release:17

package app

import java.time.LocalDateTime

import macros.MyClass

object MyFile {

  implicit val x: String = implicitly[MyClass[LocalDateTime]].xyz

}
