package scala.tools.nsc.scratchpad

import java.io.{FileInputStream, InputStreamReader, IOException}

import scala.runtime.ScalaRunTime.stringOf
import java.lang.reflect.InvocationTargetException
import scala.reflect.runtime.ReflectionUtils._
import collection.mutable.ArrayBuffer

class Mixer {

  protected val stdSeparator = "//> "
  protected val ctdSeparator = "//| "
  protected val sepColumn = 50
  protected val tabInc = 8

  type Comments = Seq[(Int, Array[Char])]

  def parseComments(comments: Array[Char]): Iterator[(Int, Array[Char])] = new Iterator[(Int, Array[Char])] {
    var idx = 0
    def hasNext = idx < comments.length
    def next() = {
      val nextSpace = comments indexOf (' ', idx)
      var nextNL = comments indexOf ('\n', nextSpace + 1)
      if (nextNL < 0) nextNL = comments.length
      val result =
        (new String(comments.slice(idx, nextSpace)).toInt, comments.slice(nextSpace + 1, nextNL))
      idx = nextNL + 1
      result
    }
  }

  def mix(source: Array[Char], comments: Array[Char]): Array[Char] = {
    val mixed = new ArrayBuffer[Char]
    var written = 0
    def align() = {
      var idx = mixed.lastIndexOf('\n') + 1
      var col = 0
      while (idx < mixed.length) {
        col =
          if (mixed(idx) == '\t') (col / tabInc) * tabInc + tabInc
          else col + 1
        idx += 1
      }
      if (col > sepColumn) {
        mixed += '\n'
        col = 0
      }
      while (col < sepColumn) {
        mixed += ' '
        col += 1
      }
    }
    for ((offset, cs) <- parseComments(comments)) {
      val sep =
        if (written < offset) {
          for (i <- written until offset) mixed += source(i)
          written = offset
          stdSeparator
        } else {
          mixed += '\n'
          ctdSeparator
        }
      align()
      mixed ++= sep ++= cs
    }
    mixed ++= source.view(written, source.length)
    mixed.toArray
  }

}

object Mixer extends Mixer {

  def contents(name: String): Array[Char] = {
    val page = new Array[Char](2 << 14)
    val buf = new ArrayBuffer[Char]
    val in = new FileInputStream(name)
    val rdr = new InputStreamReader(in)
    var nread = 0
    do {
      nread = rdr.read(page, 0, page.length)
      buf ++= (if (nread == page.length) page else page.take(nread))
    } while (nread >= 0)
    buf.toArray
  }

  def main(args: Array[String]) {
    val mixer = new Mixer
    try {
      require(args.length == 2, "required arguments: file1 file2")
      val source = contents(args(0))
      val comments = contents(args(1))
      val mixed = mixer.mix(source, comments)
      println(mixed.mkString)
    } catch {
      case ex: IOException =>
        println("error: "+ ex.getMessage)
    }
  }
}
