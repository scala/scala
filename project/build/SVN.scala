import sbt._
import java.io.{ByteArrayOutputStream}
import scala.util.matching.{Regex}

/**
 * @param root the root of an svn repository
 * @author Moix Grégory
 */
class SVN(root:Path){

  /**
   * Gets the revision number of the repository given through the constructor of the class
   * It assumes that svn is installed on the running computer.
   */
   def getRevisionNumber:Int = {
     val svnInfo = Process("svn info", root)
     var result=0
     val out= new ByteArrayOutputStream
     val code:Int = svnInfo.#>(out).!
     if(code == 0) {
         val r = out.toString
         val Pattern = new Regex("""Revision: (\d+)""","version")
         val version = Pattern.findFirstMatchIn(r)
         version match {
           case Some(s)=> result=Integer.parseInt(s.group("version"))
           case None => throw new UnableToGetRevisionNumberException
         }
     } else {
       throw new UnableToGetRevisionNumberException
     }
     result
   }
}
class UnableToGetRevisionNumberException extends RuntimeException