
// AbstractFile.scala
package test;
import java.io._;
abstract class AbstractFile {
  def path : String;
}

// PlainFile.scala
//package scala.tools.nsc.io;
//import java.io._;
class PlainFile(val file : File) extends AbstractFile {}
// VirtualFile.scala
//package scala.tools.nsc.io;
class VirtualFile(val name : String, val path : String) extends AbstractFile {
}
// ZipArchive.scala
//package scala.tools.nsc.io;
//import java.io._;
import java.util.zip._;
final class ZipArchive(val file : File, archive : ZipFile) extends PlainFile(file) {
  class Entry(name : String, path : String) extends VirtualFile(name, path) {
    override def path = "";
  }
}
