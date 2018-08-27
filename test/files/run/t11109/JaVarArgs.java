// filter: Note:
package t11109;

import java.io.*;

public class JaVarArgs {
    public <T extends Serializable> void serialize(T... ts) {}
    public <T extends Universal> void universalize(T... ts) {}
}