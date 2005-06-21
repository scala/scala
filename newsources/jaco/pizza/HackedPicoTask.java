//      /   _ _      JaCo
//  \  //\ / / \     - an ant task for PiCo
//   \//  \\_\_/
//         \         Matthias Zenger, 13/12/2001

package jaco.pizza;

import jaco.framework.ant.*;

public class HackedPicoTask extends AntCompilerTask  {
    public String compilerAdaptor() {
        return "jaco.pizza.HackedPicoAdaptor";
    }
}
