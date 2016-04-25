package test.scaladoc;

/**
 * Testing java comments. The presence of a :marker:
 * tag is verified by tests.
 */
public class JavaComments {

    /**
     * Compute the answer to the ultimate question of life, the
     * universe, and everything. :marker:
     * @param factor scaling factor to the answer
     * @return the answer to everything (42) scaled by factor
     */
    public int answer(int factor) {
	return 42 * factor;
    }

}

