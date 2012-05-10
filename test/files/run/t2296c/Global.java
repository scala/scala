package bug;

import bug.action.Action;
import java.util.List;
import java.util.LinkedList;

public class Global {
  public int items() {
    return 0;
  }

  public int items(int i) {
    return i + ls.size();
  }

  private List<Action> ls = new LinkedList<Action>();

  public void putAction(Action a) {
    a.setGlobal(this);
    ls.add(a);
  }

  public void runActions() {
    for (Action action:  ls) {
      System.out.println("RUNNING ACTION");
      action.run(0);
    }
  }
}
