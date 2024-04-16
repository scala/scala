
//> using options -Werror -Xlint -Xsource:3

import annotation.*

class LogConfig {
  val MessageFormatVersionPropX = TopicConfig_1.MESSAGE_FORMAT_VERSION_CONFIG
  @nowarn("cat=deprecation")
  val MessageFormatVersionPropY = TopicConfig_1.MESSAGE_FORMAT_VERSION_CONFIG
  @deprecated("3.0")
  val MessageFormatVersionPropZ = TopicConfig_1.MESSAGE_FORMAT_VERSION_CONFIG
  @deprecated("3.0") @nowarn("cat=deprecation")
  val MessageFormatVersionProp  = TopicConfig_1.MESSAGE_FORMAT_VERSION_CONFIG
}
