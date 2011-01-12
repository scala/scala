/*
 *  @author Stephane Micheloud
 */

object Test {

  val name = "basic"
  val host = "127.0.0.1"
  val port = 8889

  def main(args: Array[String]) {
    setenv()
    println("Server.main "+port)
    server.start()
    println("Client.main "+host+" "+port)
    client.start()
    server.terminate()
  }

  private var server = new ServerThread(port)
  private var client = new ClientThread(host, port)

  private class ServerThread(port: Int) extends Runnable {
    private var th = new Thread(this)
    def start() { th.start(); Thread.sleep(1000) }
    def run() { Server.main(Array(port.toString)) }
    def terminate() { Server.terminate(); sys.exit(0) }
  }

  private class ClientThread(host: String, port: Int) extends Runnable {
    private var th = new Thread(this)
    def start() { th.start(); th.join() }
    def run() { Client.main(Array(host, port.toString)) }
  }

  private def setenv() {
    import Env._

    // Java properties for server & client
    System.setProperty("scala.remoting.logLevel", logLevel)
    System.setProperty("java.security.manager", "")
    System.setProperty("java.security.policy", policyFile)
    // Java properties for server only
    System.setProperty("java.rmi.server.codebase", deployUrl)
    System.setProperty("java.rmi.server.hostname", host)
    System.setProperty("java.rmi.server.useCodebaseOnly", "true")

    // application-secific classes to be deployed and accessed via URL
    // (i.e. detached closure, proxy interfaces and proxy stubs)
    val classNames = List(
      "Bar$proxy",
      "Bar$proxyImpl_Stub",
      "Client$$anonfun$main$1$detach",
      "Client$proxy",
      "Client$proxyImpl_Stub",
      "Foo$proxy",
      "Foo$proxyImpl_Stub")

    val proxyImplNames =
      for (n <- classNames; i = n lastIndexOf "_Stub"; if i > 0)
      yield n.substring(0, i)

    generatePolicyFile()
    generateRmiStubs(proxyImplNames)
    generateJarFile(classNames)
  }
}

object Env {
  import java.io._, java.util.jar._

  val actors_logLevel = "0"
                   // = "3" // info+warning+error
  val logLevel = "silent"
            // = "info"     // debug user code only
            // = "info,lib" // debug user & library code

  // we assume an Apache server is running locally for deployment
  private val sep = File.separator
  val docPath = System.getProperty("user.home")+sep+"public_html"
  val docRoot = "http://127.0.0.1/~"+System.getProperty("user.name")

  private val policyTmpl =
    System.getProperty("partest.cwd")+sep+Test.name+sep+"java.policy"
  val outPath = System.getProperty("partest.output")
  val libPath = System.getProperty("partest.lib")
  val policyFile = outPath+sep+"java.policy"
  val codebaseDir = outPath+sep+"-"

  assert((new File(docPath)).isDirectory,
         "Root directory \""+docPath+"\" not found")
  val deployJar = docPath+sep+Test.name+"_deploy.jar"
  val deployUrl = docRoot+"/"+Test.name+"_deploy.jar"

  def generatePolicyFile() {
    val in = new BufferedReader(new FileReader(policyTmpl))
    val out = new PrintWriter(new BufferedWriter(new FileWriter(policyFile)))
    var line = in.readLine()
    while (line != null) {
      val line1 = line.replaceAll("@PROJECT_LIB_BASE@", codebaseDir)
      out.println(line1)
      line = in.readLine()
    }
    in.close()
    out.close()
  }

  def generateRmiStubs(classNames: List[String]) {
    val options = List(
      "-v1.2",
      "-classpath "+libPath+File.pathSeparator+outPath,
      "-d "+outPath)
    rmic(options, classNames)
    //ls(outPath)
  }

  def generateJarFile(classNames: List[String]) {
    val out = new JarOutputStream(new FileOutputStream(deployJar))
    classNames foreach (name => try {
      val classFile = name+".class"
      val in = new FileInputStream(outPath+sep+classFile)
      out putNextEntry new JarEntry(classFile)
      val buf = new Array[Byte](512)
      var len = in read buf
      while (len != -1) {
        out.write(buf, 0, len)
        len = in read buf
      }
      in.close()
    } catch {
      case e: FileNotFoundException => println(e)
    })
    out.close()
  }

  private def ls(path: String) { exec("ls -al "+path) }

  private def rmic(options: List[String], classNames: List[String]) {
    val javaHome = scala.util.Properties.javaHome
    val jdkHome =
      if (javaHome endsWith "jre") javaHome.substring(0, javaHome.length-4)
      else javaHome
    val rmicExt = if (scala.util.Properties.isWin) ".exe" else ""
    val rmicCmd = jdkHome+sep+"bin"+sep+"rmic"+rmicExt
    val cmdLine = rmicCmd+options.mkString(" ", " ", "")+
                          classNames.mkString(" "," ","")
    // println(cmdLine)
    exec(cmdLine)
  }

  private def exec(command: String) {
    val proc = Runtime.getRuntime exec command
    proc.waitFor()
    val out = new BufferedReader(new InputStreamReader(proc.getInputStream))
    var line = out.readLine()
    while (line != null) {
      println(line)
      line = out.readLine()
    }
    out.close()
    val err = new BufferedReader(new InputStreamReader(proc.getErrorStream))
    line = err.readLine()
    while (line != null) {
      println(line)
      line = err.readLine()
    }
    err.close()
  }
}
