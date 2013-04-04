class ImplicitParam {
    def foo(i: Int)(implicit f: Float, d: Double) = 42
}
