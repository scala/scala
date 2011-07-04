package scala.tools.nsc.scratchpad

import java.io.{PrintStream, OutputStreamWriter, Writer}

object Executor {

  println("exec started")

  def execute(name: String, si: SourceInserter) {
    val oldOut = System.out
    val oldErr = System.err
    val cwr = new CommentWriter(si)
    val newOut = new PrintStream(new CommentOutputStream(cwr))
    java.lang.System.setOut(newOut)
    java.lang.System.setErr(newOut)

    try {
      Class.forName(name).newInstance()
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
    } finally {
      cwr.close()
      System.setOut(oldOut)
      System.setErr(oldErr)
    }
  }
}