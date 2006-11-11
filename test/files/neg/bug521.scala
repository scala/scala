package test

import java.io.File
import java.util.zip.ZipFile

abstract class AbstractFile {
  def path : String;
}

class PlainFile(val file : File) extends AbstractFile {}
class VirtualFile(val name : String, val path : String) extends AbstractFile {}

final class ZipArchive(val file : File, archive : ZipFile) extends PlainFile(file) {
  class Entry(name : String, path : String) extends VirtualFile(name, path) {
    override def path = "";
  }
}
