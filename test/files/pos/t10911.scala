object OfFor {top =>
	trait Something
	trait Tag[X]
	trait Template[T] {
		type Repr
		trait Tag extends top.Tag[Repr]
	}
	type OfFor[T, _Repr] = Template[T] {
		type Repr = _Repr
	}

	implicit def something[T : Tag]: Something = ???
	implicit def hkTag[T, Repr[X]]: OfFor[T, Repr[T]]#Tag = ???
	implicitly[Something]
}
