import sbt._

import Build._
import Keys._
import Project.Initialize
import scala.collection.{ mutable, immutable }
import scala.collection.parallel.CompositeThrowable
import java.security.MessageDigest


/** Helpers to resolve SHA artifacts from typesafe repo. */
object ShaResolve {
  import dispatch.{Http,url}
  val remote_urlbase="http://typesafe.artifactoryonline.com/typesafe/scala-sha-bootstrap/org/scala-lang/bootstrap"  
  
  val pullBinaryLibs = TaskKey[Unit]("pull-binary-libs", "Pulls binary libs by the SHA key.")
  val pushBinaryLibs = TaskKey[Unit]("push-binary-libs", "Pushes binary libs whose SHA has changed.")
  val binaryLibCache = SettingKey[File]("binary-lib-cache", "Location of the cache of binary libs for this scala build.")

  def settings: Seq[Setting[_]] = Seq(
    binaryLibCache in ThisBuild := file(System.getProperty("user.home")) / ".sbt" / "cache" / "scala",
    pullBinaryLibs in ThisBuild <<= (baseDirectory, binaryLibCache, streams) map resolveLibs
  )

  def resolveLibs(dir: File, cacheDir: File, s: TaskStreams): Unit = loggingParallelExceptions(s) {
     val files = (dir / "test" / "files" ** "*.desired.sha1") +++ (dir / "lib" ** "*.desired.sha1")
     for {
       (file, name) <- (files x relativeTo(dir)).par
       uri = name.dropRight(13).replace('\\', '/')       
       jar = dir / uri
       if !jar.exists || !isValidSha(file)
       sha = getShaFromShafile(file)
     } pullFile(jar, sha + "/" + uri, cacheDir, sha, s)
  }

  @inline final def loggingParallelExceptions[U](s: TaskStreams)(f: => U): U = try f catch {
    case t: CompositeThrowable =>
      s.log.error("Error during parallel execution, GET READY FOR STACK TRACES!!")
      t.throwables foreach (t2 => s.log.trace(t2))
      throw t
  }

  // TODO - Finish this publishing aspect.

  def getShaFromShafile(file: File): String = parseShaFile(file)._2

  // This should calculate the SHA sum of a file the same as the linux process.
  def calculateSha(file: File): String = {
    val digest = MessageDigest.getInstance("SHA1")
    val in = new java.io.FileInputStream(file);
    val buffer = new Array[Byte](8192)
    try {
       def read(): Unit = in.read(buffer) match {
         case x if x <= 0 => ()
         case size => digest.update(buffer, 0, size); read()
       }
       read()
    } finally in.close()
    val sha = convertToHex(digest.digest())
    sha
  }

  // TODO - Prettier way of doing this...
  private def convertToHex(data: Array[Byte]): String = { 
    val buf = new StringBuffer
    for (i <- 0 until data.length) { 
      var halfbyte = (data(i) >>> 4) & 0x0F;
      var two_halfs = 0;
      while(two_halfs < 2) { 
        if ((0 <= halfbyte) && (halfbyte <= 9)) 
          buf.append(('0' + halfbyte).toChar)
        else 
          buf.append(('a' + (halfbyte - 10)).toChar);
        halfbyte = data(i) & 0x0F;
        two_halfs += 1
      }
    } 
    return buf.toString
  } 

  // Parses a sha file into a file and a sha.
  def parseShaFile(file: File): (File, String) =
    IO.read(file).split("\\s") match {
       case Array(sha, filename) if filename.startsWith("?") => (new File(file.getParentFile, filename.drop(1)), sha)
       case Array(sha, filename)                             => (new File(file.getParentFile, filename), sha)
       case _                                                => error(file.getAbsolutePath + " is an invalid sha file")
    }
  

  def isValidSha(file: File): Boolean =
    try {
      val (jar, sha) = parseShaFile(file)
      jar.exists && calculateSha(jar) == sha
    } catch {
      case t: Exception => false
    }
     

  def pullFile(file: File, uri: String, cacheDir: File, sha: String, s: TaskStreams): Unit = {
    val cachedFile = cacheDir / uri
    if (!cachedFile.exists || calculateSha(cachedFile) != sha) {
      // Ensure the directory for the cache exists.
      cachedFile.getParentFile.mkdirs()
      val url = remote_urlbase + "/" + uri
      val fous = new java.io.FileOutputStream(cachedFile)
      s.log.info("Pulling [" + cachedFile + "] to cache")
      try Http(dispatch.url(url) >>> fous) finally fous.close()
    }
    s.log.info("Pulling [" + file + "] from local cache")
    IO.copyFile(cachedFile, file)
  }
  
  def pushFile(file: File, uri: String, user: String, pw: String): Unit = {
    val url = remote_urlbase + "/" + uri
    val sender = dispatch.url(url).PUT.as(user,pw) <<< (file, "application/java-archive")
    // TODO - output to logger.
    Http(sender >>> System.out)
  }
}
