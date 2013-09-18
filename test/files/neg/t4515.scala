import scala.collection.mutable.HashMap

object Main {
  trait Target { }

  trait PushEventContext[EventType] {
    def getEvent: EventType
  }
  trait PushNode[EventType] { }
  trait DerivedPushNode[EventType] extends PushNode[EventType] { }

  trait HandlerBase[EventType] {
    def onEvent(target: Target,
                event: EventType,
                node: PushNode[EventType],
                ctx: PushEventContext[EventType]): Unit
                                            }
  val handlers = new HashMap[DerivedPushNode[_], HandlerBase[_]]

  object TimerPushService {
    private val INSTANCE: TimerPushService = new TimerPushService
    def get: TimerPushService = INSTANCE
  }

  class TimerPushService {
    def add[EventType](node: DerivedPushNode[EventType],
                       context: PushEventContext[EventType]): Unit = {}

    def pollEvents[EventType](node: DerivedPushNode[EventType]): List[PushEventContext[EventType]] =
      Nil
  }

  def onTimer(target: Target) {
    val pushService = TimerPushService.get
    for ((node, handler) <- handlers) {
      for (ctx <- pushService.pollEvents(node)) {
        handler.onEvent(target, ctx.getEvent, node, ctx)
      }
    }
  }
}