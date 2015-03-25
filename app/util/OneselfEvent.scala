package util

case class OneselfEvent(source: String,
                        version: String,
                        properties: OneselfProperty,
                        dateTime: String,
                        latestSyncField: String,
                        objectTags: Array[String],
                        actionTags: Array[String]
                         )
