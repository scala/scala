package com.mosol.sl;

case class Span[K <: Ordered[K]](low: Option[K], high: Option[K]) extends Function1[K, boolean] {
  def apply(k: K): boolean = this match {
    case Span(Some(low), Some(high)) => (k >= low && k <= high)
    case Span(Some(low), None) => (k >= low)
    case Span(None, Some(high)) => (k <= high)
    case _ => false
  }
}

