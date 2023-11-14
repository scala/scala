package foo

import scala.util.Random

trait Base {
  private val myRandom = Random.nextInt(100)
  def somePublicMethod(): Int = 123
}