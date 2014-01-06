package bug.action;

import bug.Global;

public abstract class Action {
  protected Global m_glob;

  public Action(Global glob0) {
    m_glob = glob0;
  }

  public Action() {
    this(null);
  }

  public abstract void run(int v);

  public void setGlobal(Global g) {
    m_glob = g;
  }
}
