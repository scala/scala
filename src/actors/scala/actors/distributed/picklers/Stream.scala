package scala.actors.distributed.picklers;

import java.io.Reader;
import java.io.Writer;

import scala.collection.mutable.HashMap;

abstract class Stream {
  protected var loc: int = 0;

  def getLocation = loc;
}

class OutStream(writer: Writer) extends Stream {
  val picklerEnv = new PicklerEnv;

  def write(s: String): unit = {
    loc = loc + s.length()
    writer.write(s)
  }

  def write(c: char): unit = {
    loc = loc + 1
    writer.write(c)
  }

  def flush(): unit =
    writer.flush();
}

class InStream(reader: Reader) extends Stream {
  val unpicklerEnv = new UnpicklerEnv;

  def read(num: int): String = {
    val carr = new Array[char](num)
    val cnt = reader.read(carr)
    loc = loc + num
    //Console.println("new loc: " + loc)
    new String(carr)

    /*val buf = new StringBuffer
    var ch = r.read()
    loc = loc + 1
    if (num > 1) {
      var cnt = 1
      while (cnt < num && ch != -1) {
        buf.append(ch)
        ch = r.read()
        loc = loc + 1
        cnt = cnt + 1
      }
      if (cnt == num)
        buf.toString()
      else
        buf.toString() // error
    }
    else
      new String(ch.asInstanceOf[char])*/
  }

  def read(cbuf: Array[char]): int = {
    loc = loc + cbuf.length
    reader.read(cbuf)
  }

  def readChar: char = {
    val carr = new Array[char](1)
    read(carr)
    carr(0)
  }
}

class PicklerEnv[a] extends HashMap[a, int] {
  private var cnt: int = 0;
  def nextLoc() = { cnt = cnt + 1; cnt };
}

class UnpicklerEnv[a] extends HashMap[int, a] {
  private var cnt: int = 0;
  def nextLoc() = { cnt = cnt + 1; cnt };
}
