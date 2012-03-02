// /scala/trac/5529/a.scala
// Tue Feb 28 13:11:28 PST 2012

package test;

object Test {
  sealed class File {
    val i = 1
  }
  sealed class Dir extends File { }

  type File
}
