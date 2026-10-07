
public interface J {
    void f(E e);

    enum E {
        X {
            public void g() { return; }
	};
        abstract public void g();
    }
}
