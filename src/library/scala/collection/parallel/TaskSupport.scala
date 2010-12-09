package scala.collection.parallel







trait TaskSupport extends Tasks

class ForkJoinTaskSupport extends TaskSupport with AdaptiveWorkStealingForkJoinTasks

class ThreadPoolTaskSupport extends TaskSupport with AdaptiveWorkStealingThreadPoolTasks


















