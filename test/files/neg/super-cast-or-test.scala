trait A { def f = super.asInstanceOf[AnyRef] }
trait B { def g = super.isInstanceOf[AnyRef] }

