package scala.reflect
package internal

trait AbstractFileApi {
  def path: String
  def canonicalPath: String
}
