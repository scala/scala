package scala.concurrent.util.duration

import scala.concurrent.util.{ FiniteDuration }

trait Classifier[C] {
  type R
  def convert(d: FiniteDuration): R
}

