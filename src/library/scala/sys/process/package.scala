/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.sys

package object process extends ProcessImplicits {
  // These are in a nested object instead of at the package level
  // due to the issues described in tickets #3160 and #3836.
  private[process] object processAliases {
    type Closeable       = java.io.Closeable
    type File            = java.io.File
    type IOException     = java.io.IOException
    type InputStream     = java.io.InputStream
    type JProcess        = java.lang.Process
    type JProcessBuilder = java.lang.ProcessBuilder
    type OutputStream    = java.io.OutputStream
    type SyncVar[T]      = scala.concurrent.SyncVar[T]
    type URL             = java.net.URL
  }
}
