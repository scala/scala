import sbt._

import Build._
import Keys._
import Project.Initialize
import scala.collection.{ mutable, immutable }
import scala.collection.parallel.CompositeThrowable
import java.security.MessageDigest

case class Credentials(user: String, pw: String)

/** Helpers to resolve SHA artifacts from typesafe repo. */
object ShaResolve {
  import dispatch.{Http,url}
  val remote_urlbase="http://typesafe.artifactoryonline.com/typesafe/scala-sha-bootstrap/org/scala-lang/bootstrap"  
  
  val pullBinaryLibs = TaskKey[Unit]("pull-binary-libs", "Pulls binary libs by the SHA key.")
  val pushBinaryLibs = TaskKey[Unit]("push-binary-libs", "Pushes binary libs whose SHA has changed.")
  val binaryLibCache = SettingKey[File]("binary-lib-cache", "Location of the cache of binary libs for this scala build.")

  def settings: Seq[Setting[_]] = Seq(
    binaryLibCache in ThisBuild := file(System.getProperty("user.home")) / ".sbt" / "cache" / "scala",
    pullBinaryLibs in ThisBuild <<= (baseDirectory, binaryLibCache, streams) map resolveLibs,
    pushBinaryLibs in ThisBuild <<= (baseDirectory, streams) map getCredentialsAndPushFiles
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

  /** This method removes all SHA1 files that don't match their corresponding JAR. */
  def removeInvalidShaFiles(dir: File): Unit = {
    val files = (dir / "test" / "files" ** "*.desired.sha1") +++ (dir / "lib" ** "*.desired.sha1")
    for {
      (file, name) <- (files x relativeTo(dir)).par
      uri = name.dropRight(13).replace('\\', '/')       
      jar = dir / uri
      if !jar.exists || !isValidSha(file)
    } IO.delete(jar)
  }
  def getCredentials: Credentials = System.out.synchronized {
    val user = (SimpleReader.readLine("Please enter your STARR username> ") getOrElse error("No username provided."))
    val password = (SimpleReader.readLine("Please enter your STARR password> ", Some('*')) getOrElse error("No password provided."))
    Credentials(user, password)
  }

  def getCredentialsAndPushFiles(dir: File, s: TaskStreams): Unit =
    pushFiles(dir, getCredentials, s)

  def pushFiles(dir: File, cred: Credentials, s: TaskStreams): Unit = loggingParallelExceptions(s) {
    val files = (dir / "test" / "files" ** "*.jar") +++ (dir / "lib" ** "*.jar")
    for {
      (jar, name) <- (files x relativeTo(dir)).par
      shafile = dir / (name + ".desired.sha1")
      if !shafile.exists || !isValidSha(shafile)
    } pushFile(jar, name, cred, s)
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

  def convertToHex(data: Array[Byte]): String = {
    def byteToHex(b: Int) =
      if ((0 <= b) && (b <= 9)) ('0' + b).toChar
      else ('a' + (b-10)).toChar
    val buf = new StringBuffer
    for (i <- 0 until data.length) {
      buf append byteToHex((data(i) >>> 4) & 0x0F)
      buf append byteToHex(data(i) & 0x0F)
    }
    buf.toString
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
  
  // Pushes a file and writes the new .desired.sha1 for git.
  def pushFile(file: File, uri: String, cred: Credentials, s: TaskStreams): Unit = {
    val sha = calculateSha(file)
    val url = remote_urlbase + "/" + sha + "/" + uri
    val sender = dispatch.url(url).PUT.as(cred.user,cred.pw) <<< (file, "application/java-archive")
    // TODO - output to logger.
    Http(sender >>> System.out)
    val shafile = file.getParentFile / (file.getName + ".desired.sha1")
    IO.touch(shafile)
    IO.write(shafile, sha + " ?" + file.getName)
  }
}
