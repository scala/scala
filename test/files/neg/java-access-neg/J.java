package a.b;

public abstract class J {
  public J() { }
  J(int x1) { }
  protected J(int x1, int x2) { }
  
  abstract void packageAbstract();
  protected abstract void protectedAbstract();
  public abstract void publicAbstract();
  
  void packageConcrete() { return; }
  protected void protectedConcrete() { return; }
  public void publicConcrete() { return; }
}
