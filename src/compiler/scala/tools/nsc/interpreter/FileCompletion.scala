/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc
package interpreter

/** TODO
 *  Spaces, dots, and other things in filenames are not correctly handled.
 *  space-escaping, knowing when we're inside quotes, etc. would be nice.
 */

import io.{ Directory, Path }

/** This isn't 100% clean right now, but it works and is simple.  Rather
 *  than delegate to new objects on each '/' in the path, we treat the
 *  buffer like a path and process it directly.
 */
object FileCompletion {
  def executionFor(buffer: String): Option[Path] = {
    Some(Directory.Home match {
      case Some(d) if buffer startsWith "~" => d / buffer.tail
      case _                                => Path(buffer)
    }) filter (_.exists)
  }

  private def fileCompletionForwarder(buffer: String, where: Directory): List[String] = {
    completionsFor(where.path + buffer) map (_ stripPrefix where.path) toList
  }

  private def homeCompletions(buffer: String): List[String] = {
    require(buffer startsWith "~/")
    val home = Directory.Home getOrElse (return Nil)
    fileCompletionForwarder(buffer.tail, home) map ("~" + _)
  }
  private def cwdCompletions(buffer: String): List[String] = {
    require(buffer startsWith "./")
    val cwd = Directory.Current getOrElse (return Nil)
    fileCompletionForwarder(buffer.tail, cwd) map ("." + _)
  }

  def completionsFor(buffer: String): List[String] =
    if (buffer startsWith "~/") homeCompletions(buffer)
    else if (buffer startsWith "./") cwdCompletions(buffer)
    else {
      val p = Path(buffer)
      val (dir, stub) =
        // don't want /foo/. expanding "."
        if (p.name == ".") (p.parent, ".")
        else if (p.isDirectory) (p.toDirectory, "")
        else (p.parent, p.name)

      dir.list filter (_.name startsWith stub) map (_.path) toList
    }
}