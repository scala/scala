
package scala.actors;

/**
 * IFJTaskRunnerGroup
 *
 * @version 0.9.8
 * @author Philipp Haller
 */
interface IFJTaskRunnerGroup {
    public void executeTask(FJTask t);
    public FJTaskRunner[] getArray();
    public FJTask pollEntryQueue();
    public void setActive(FJTaskRunner t);
    public void checkActive(FJTaskRunner t, long scans);
    public void setInactive(FJTaskRunner t);
    public LinkedQueue getEntryQueue();
}
