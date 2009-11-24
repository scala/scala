package pkg1 {
    class C {
        private[pkg1] def foo: Int = 1
    }

    trait T extends C {
        private[pkg1] abstract override def foo = super.foo + 1
    }
}
