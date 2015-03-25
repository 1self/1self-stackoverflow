package util

case class OneselfSyncEvent(
                             properties: OneselfSyncProperty,
                             dateTime: String,
                             objectTags: Array[String],
                             actionTags: Array[String]
                             )
