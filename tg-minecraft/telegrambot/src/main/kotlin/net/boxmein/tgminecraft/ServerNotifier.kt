package net.boxmein.tgminecraft

import com.pusher.rest.Pusher

const val CHANNEL: String = "tgminecraft"
const val EVENT_TYPE: String = "minecraft-command"

val PUSHER_APP_ID: String = System.getenv("PUSHER_APP_ID") ?: ""
val PUSHER_APP_KEY: String = System.getenv("PUSHER_APP_KEY") ?: ""
val PUSHER_APP_SECRET: String = System.getenv("PUSHER_APP_SECRET") ?: ""

class ServerNotifier {
  val pusher = Pusher(
    PUSHER_APP_ID,
    PUSHER_APP_KEY,
    PUSHER_APP_SECRET
  )

  init {
    pusher.setCluster("eu")
    pusher.setEncrypted(true)
  }

  fun triggerMessage(msg: String) {
    pusher.trigger(CHANNEL, EVENT_TYPE, msg)
  }
}
