package scala.tools.partest
package nest

import java.io.File
import scala.tools.nsc.io.{ Directory }


class SBTRunner extends DirectRunner {

  val fileManager = new FileManager {
    var JAVACMD: String = "java"
    var JAVAC_CMD: String = "javac"
    var CLASSPATH: String = _
    var LATEST_LIB: String = _
    val testRootPath: String = "test"
    val testRootDir: Directory = Directory(testRootPath)
  }

  def reflectiveRunTestsForFiles(kindFiles: Array[File], kind: String):java.util.HashMap[String,Int] = {

    def convert(scalaM:scala.collection.immutable.Map[String,Int]):java.util.HashMap[String,Int] = {
      val javaM = new java.util.HashMap[String,Int]()
      for(elem <- scalaM) yield {javaM.put(elem._1,elem._2)}
      javaM
    }

    def failedOnlyIfRequired(files:List[File]):List[File]={
      if (fileManager.failed) files filter (x => fileManager.logFileExists(x, kind)) else files
    }

    convert(runTestsForFiles(failedOnlyIfRequired(kindFiles.toList), kind))

  }
}

