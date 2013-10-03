import java.io.{File, FileOutputStream, BufferedOutputStream, FileWriter, ByteArrayOutputStream, PrintStream}
import tools.nsc.{CompileClient, CompileServer}
import java.util.concurrent.{CountDownLatch, TimeUnit}

object Test extends App {
  val startupLatch = new CountDownLatch(1)
  // we have to explicitly launch our server because when the client launches a server it uses
  // the "scala" shell command meaning whatever version of scala (and whatever version of libraries)
  // happens to be in the path gets used
  val t = new Thread(new Runnable {
    def run() = {
      CompileServer.execute(() => startupLatch.countDown(), Array[String]())
    }
  })
  t setDaemon true
  t.start()
  if (!startupLatch.await(2, TimeUnit.MINUTES))
    sys error "Timeout waiting for server to start"

  val baos = new ByteArrayOutputStream()
  val ps = new PrintStream(baos)

  val outdir = scala.reflect.io.Directory(sys.props("partest.output"))

  val dirNameAndPath = (1 to 2).toList map {number =>
    val name = s"Hello${number}"
    val dir = outdir / number.toString
    (dir, name, dir / s"${name}.scala")
  }

  dirNameAndPath foreach {case (dir, name, path) =>
    dir.createDirectory()
    val file = path.jfile
    val out = new FileWriter(file)
    try
      out.write(s"object ${name}\n")
    finally
      out.close
  }

  val success = (scala.Console withOut ps) {
    dirNameAndPath foreach {case (path, name, _) =>
      CompileClient.process(Array("-verbose", "-current-dir", path.toString, s"${name}.scala"))
    }

    CompileClient.process(Array("-shutdown"))
  }

  // now make sure we got success and the correct normalized paths
  val msg = baos.toString()

  assert(success, s"got a failure. Full results were: \n${msg}")
  dirNameAndPath foreach {case (_, _, path) =>
    val expected = s"Input files after normalizing paths: ${path}"
    assert(msg contains expected, s"could not find '${expected}' in output. Full results were: \n${msg}")
  }
}
