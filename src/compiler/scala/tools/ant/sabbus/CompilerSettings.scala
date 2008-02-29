/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant.sabbus

import java.io.File

import org.apache.tools.ant.types.{Path, Reference}

class CompilerSettings {

  private var gBf: Option[String] = None
  def g = gBf.get
  def g_=(s: String): this.type = { gBf = Some(s); this }

  private var uncheckedBf: Boolean = false
  def unchecked = uncheckedBf
  def unchecked_=(b: Boolean): this.type = { uncheckedBf = b; this }

  private var classpathBf: Option[Path] = None
  def classpath = classpathBf.get
  def classpath_=(p: Path): this.type = { classpathBf = Some(p); this }

  private var sourcepathBf: Option[Path] = None
  def sourcepath = sourcepathBf.get
  def sourcepath_=(p: Path): this.type = { sourcepathBf = Some(p); this }

  private var bootclasspathBf: Option[Path] = None
  def bootclasspath = bootclasspathBf.get
  def bootclasspath_=(p: Path): this.type = { bootclasspathBf = Some(p); this }

  private var extdirsBf: Option[Path] = None
  def extdirs = extdirsBf.get
  def extdirs_=(p: Path): this.type = { extdirsBf = Some(p); this }

  private var dBf: Option[File] = None
  def d = dBf.get
  def d_=(f: File): this.type = { dBf = Some(f); this }

  private var encodingBf: Option[String] = None
  def encoding = encodingBf.get
  def encoding_=(s: String): this.type = { encodingBf = Some(s); this }

  private var targetBf: Option[String] = None
  def target = targetBf.get
  def target_=(s: String): this.type = { targetBf = Some(s); this }

  private var optimiseBf: Boolean = false
  def optimise = optimiseBf
  def optimise_=(b: Boolean): Unit = { optimiseBf = b }

  private var moreBf: Option[String] = None
  def more = moreBf.get
  def more_=(s: String): this.type = { moreBf = Some(s); this }

  def toArgs: String = ("" +
    (if (!gBf.isEmpty) "-g:" + g + " " else "") +
    (if (uncheckedBf) "-unchecked " else "") +
    (if (!classpathBf.isEmpty) "-classpath " + classpath + " " else "") +
    (if (!sourcepathBf.isEmpty) "-sourcepath " + sourcepath + " " else "") +
    (if (!bootclasspathBf.isEmpty) "-bootclasspath " + bootclasspath + " " else "") +
    (if (!extdirsBf.isEmpty) "-extdirs " + extdirs + " " else "") +
    (if (!dBf.isEmpty) "-d " + d + " " else "") +
    (if (!encodingBf.isEmpty) "-encoding " + encoding + " " else "") +
    (if (!targetBf.isEmpty) "-target:" + target + " " else "") +
    (if (optimiseBf) "-optimise " else "") +
    (if (!moreBf.isEmpty) more else "")
  )

  override def equals(that: Any): Boolean = that match {
    case cs: CompilerSettings =>
      this.gBf == cs.gBf &&
      this.uncheckedBf == cs.uncheckedBf &&
      this.classpathBf == cs.classpathBf &&
      this.sourcepathBf == cs.sourcepathBf &&
      this.bootclasspathBf == cs.bootclasspathBf &&
      this.extdirsBf == cs.extdirsBf &&
      this.dBf == cs.dBf &&
      this.encodingBf == cs.encodingBf &&
      this.targetBf == cs.targetBf &&
      this.optimiseBf == cs.optimiseBf &&
      this.moreBf == cs.moreBf
  }

}
