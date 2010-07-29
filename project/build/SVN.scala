import sbt._
import java.io.{ByteArrayOutputStream}
import scala.util.matching.{Regex}

/**
 * @param root the root of an svn repository
 * @author Moix Grégory
 */
class SVN(root: Path) {
  /** Location of tool which parses svn revision in git-svn repository. */
  val GitSvnRevTool = root / "tools" / "get-git-svn-rev"

  /**
   * Gets the revision number of the repository given through the constructor of the class
   * It assumes that svn is installed on the running computer.
   */
  def getRevisionNumber: Int = getSvn orElse getGit getOrElse {
    throw new UnableToGetRevisionNumberException
  }
  def getSvn: Option[Int] = {
    val svnInfo = Process("svn info", root)
    val out = new ByteArrayOutputStream
    val code:Int = svnInfo.#>(out).!

    if (code == 0) {
      val r = out.toString
      val Pattern = new Regex("""Revision: (\d+)""", "version")
      val version = Pattern.findFirstMatchIn(r)

      version map (_.group("version").toInt)
    }
    else None
  }

  def getGit: Option[Int] =
    try   { Some(Process(GitSvnRevTool.toString, root).!!.trim.toInt) }
    catch { case _: Exception => None }
}

class UnableToGetRevisionNumberException extends RuntimeException