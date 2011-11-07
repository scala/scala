package com.mosol.sl

case class Span[K <: Ordered[K]](low: Option[K], high: Option[K]) extends Function1[K, Boolean] {
  override def equals(x$1: Any): Boolean = x$1 match {
    case Span((low$0 @ _), (high$0 @ _)) if low$0.equals(low).$amp$amp(high$0.equals(high)) => true
    case _ => false
  }
  def apply(k: K): Boolean = this match {
    case Span(Some(low), Some(high)) => (k >= low && k <= high)
    case Span(Some(low), None) => (k >= low)
    case Span(None, Some(high)) => (k <= high)
    case _ => false
  } 
}
