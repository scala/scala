package test;

import static test.A.STATE.UNDEF;

class A {

    public STATE state = UNDEF;

	protected static enum STATE {
		UNDEF
	}

}
