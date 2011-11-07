/** process tests. 
 */

import java.io.{ File, FileNotFoundException, IOException, InputStream, OutputStream, FileInputStream }
import java.net.{ URI, URISyntaxException, URL }
import org.scalacheck._
import Prop._
import sys.process._
import scala.tools.nsc.io.{ File => SFile }

/** This has scrounged bits of sbt to flesh it out enough to run.
 */
package processtest {
  
  object exit
  {
    def fn(code: Int) = System.exit(code)
  	def main(args: Array[String]) = exit.fn(java.lang.Integer.parseInt(args(0)))
  }
  object cat
  {
  	def main(args: Array[String])
  	{
      try {
       if (args.length == 0)
         IO.transfer(System.in, System.out)
       else
         catFiles(args.toList)
       exit.fn(0)
      } catch {
       case e =>
         e.printStackTrace()
         System.err.println("Error: " + e.toString)
         exit.fn(1)
      }
  	}
  	private def catFiles(filenames: List[String]): Option[String] = filenames match {
       case head :: tail =>
         val file = new File(head)
         if (file.isDirectory)
           throw new IOException("Is directory: " + file)
         else if (file.exists) {
           IO.transfer(file, System.out)
           catFiles(tail)
         }
         else
           throw new FileNotFoundException("No such file or directory: " + file)
       case Nil => None
  	}
  }
  object echo
  {
  	def main(args: Array[String])
  	{
  		System.out.println(args.mkString(" "))
  	}
  }
}

object IO {
  def transfer(in: InputStream, out: OutputStream): Unit = BasicIO.transferFully(in, out)
  def transfer(in: File, out: OutputStream): Unit        = BasicIO.transferFully(new FileInputStream(in), out)

	def classLocation(cl: Class[_]): URL = {
		val codeSource = cl.getProtectionDomain.getCodeSource
		if(codeSource == null) sys.error("No class location for " + cl)
		else codeSource.getLocation
	}
	def classLocationFile(cl: Class[_]): File = toFile(classLocation(cl))
	def classLocation[T](implicit mf: Manifest[T]): URL = classLocation(mf.erasure)
	def classLocationFile[T](implicit mf: Manifest[T]): File = classLocationFile(mf.erasure)

	def toFile(url: URL) =
		try { new File(url.toURI) }
		catch { case _: URISyntaxException => new File(url.getPath) }
}

class ProcessSpecification extends Properties("Process I/O") {
	implicit val exitCodeArb: Arbitrary[Array[Byte]] = Arbitrary(Gen.choose(0, 10) flatMap { size =>
	  Gen.resize(size, Arbitrary.arbArray[Byte].arbitrary) 
	})

	/*property("Correct exit code") = forAll( (exitCode: Byte) => checkExit(exitCode))
	property("#&& correct") = forAll( (exitCodes: Array[Byte]) => checkBinary(exitCodes)(_ #&& _)(_ && _))
	property("#|| correct") = forAll( (exitCodes: Array[Byte]) => checkBinary(exitCodes)(_ #|| _)(_ || _))
	property("### correct") = forAll( (exitCodes: Array[Byte]) => checkBinary(exitCodes)(_ ### _)( (x,latest) => latest))*/
	property("Pipe to output file") = forAll( (data: Array[Byte]) => checkFileOut(data))
	property("Pipe to input file") = forAll( (data: Array[Byte]) => checkFileIn(data))
	property("Pipe to process") = forAll( (data: Array[Byte]) => checkPipe(data))

	private def checkBinary(codes: Array[Byte])(reduceProcesses: (ProcessBuilder, ProcessBuilder) => ProcessBuilder)(reduceExit: (Boolean, Boolean) => Boolean) =
	{
		(codes.length > 1) ==>
		{
			val unsignedCodes = codes.map(unsigned)
			val exitCode = unsignedCodes.map(code => Process(process("processtest.exit " + code))).reduceLeft(reduceProcesses) !
			val expectedExitCode = unsignedCodes.map(toBoolean).reduceLeft(reduceExit)
			toBoolean(exitCode) == expectedExitCode
		}
	}
	private def toBoolean(exitCode: Int) = exitCode == 0
	private def checkExit(code: Byte) =
	{
		val exitCode = unsigned(code)
		(process("processtest.exit " + exitCode) !) == exitCode
	}
	private def checkFileOut(data: Array[Byte]) =
	{
		withData(data) { (temporaryFile, temporaryFile2) =>
			val catCommand = process("processtest.cat " + temporaryFile.getAbsolutePath)
			catCommand #> temporaryFile2
		}
	}
	private def checkFileIn(data: Array[Byte]) =
	{
		withData(data) { (temporaryFile, temporaryFile2) =>
			val catCommand = process("processtest.cat")
			temporaryFile #> catCommand #> temporaryFile2
		}
	}
	private def checkPipe(data: Array[Byte]) =
	{
		withData(data) { (temporaryFile, temporaryFile2) =>
			val catCommand = process("processtest.cat")
			temporaryFile #> catCommand #| catCommand #> temporaryFile2
		}
	}
	private def temp() = SFile(File.createTempFile("processtest", ""))
	private def withData(data: Array[Byte])(f: (File, File) => ProcessBuilder) =
	{
		val temporaryFile1 = temp()
		val temporaryFile2 = temp()
		try {
		  temporaryFile1 writeBytes data
			val process = f(temporaryFile1.jfile, temporaryFile2.jfile)
			( process ! ) == 0 &&
			{
				val b1 = temporaryFile1.slurp()
				val b2 = temporaryFile2.slurp()
				b1 == b2
			}
		}
		finally
		{
			temporaryFile1.delete()
			temporaryFile2.delete()
		}
	}
	private def unsigned(b: Byte): Int = ((b: Int) +256) % 256
	private def process(command: String) = {
		val thisClasspath = List(getSource[ScalaObject], getSource[IO.type], getSource[SourceTag]).mkString(File.pathSeparator)
		"java -cp " + thisClasspath + " " + command
	}
	private def getSource[T : Manifest]: String =
		IO.classLocationFile[T].getAbsolutePath
}
private trait SourceTag


object Test extends ProcessSpecification { }
