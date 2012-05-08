package scala.reflect
package api

trait RequiredFile {
  def path: String
  def canonicalPath: String
}
