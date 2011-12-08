/* NSC -- new Scala compiler
* Copyright 2011 LAMP/EPFL
* @author Simon Ochsenreither
*/

package scala.tools.nsc.settings

object Version {
  private var separators = Array('.', '-', '~')
  def apply(version: String) = new Version(version.split(separators).map(_.toInt): _*)

  def apply(version: String, separators: Char*) =
    new Version(version.split(separators.toArray).map(_.toInt): _*)
}

case class Version(version: Int*) extends Ordered[Version] {
  def compare(that: Version): Int = {
    this.version.zipAll(that.version, 0, 0)
      .foreach {
      v => if (v._1 < v._2) return -1 else if (v._1 > v._2) return 1
    }

    return 0
  }
  override def toString = version.mkString(".")
}
