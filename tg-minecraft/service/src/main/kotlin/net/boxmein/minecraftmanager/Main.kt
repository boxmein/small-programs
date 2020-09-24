package net.boxmein.minecraftmanager
import kotlin.concurrent.*

import com.pusher.client.Pusher
import com.pusher.client.channel.Channel
import com.pusher.client.PusherOptions
import com.pusher.client.connection.ConnectionEventListener
import com.pusher.client.connection.ConnectionState
import com.pusher.client.connection.ConnectionStateChange

const val CHANNEL: String = "tgminecraft"
const val EVENT_TYPE: String = "minecraft-command"

val RCON_MATCHER: Regex = "^RCON (.+?)$".toRegex()

fun main() {
  println("Hello, service")
  
  val manager = MinecraftManager()
  thread {
    manager.start()
    // .start() blocks until the minecraft manager process stops
  }

  val pusher = connectToPusher()
  pusher.bind(EVENT_TYPE) { event ->
    val command = event.data
    println("\u001b[31;1mCommand received: ${command}\u001b[0m")
    if (command == "STOP") {
      manager.stop()
    } else if (command == "SAVE") {
      manager.saveAll()
    } else if (command matches RCON_MATCHER) {
      val matches = RCON_MATCHER.find(command)
      matches?.let {
        if (matches.groupValues.size == 2) {
          val cmd = matches.groupValues[1]
          manager.sendConsoleCommand(cmd)
        }
      }
    }
  }

  Runtime.getRuntime().addShutdownHook(Thread {
    manager.stop()
    manager.process?.waitFor()
  })
}

fun connectToPusher(): Channel {
  val options = PusherOptions()
  options.setCluster("eu")

  val pusher = Pusher("47db97bc033d8ab36649", options)
  pusher.connect(object: ConnectionEventListener {
    override fun onConnectionStateChange(change: ConnectionStateChange) {
      println("Pusher: connection state = ${change.currentState}")
    }
    override fun onError(
      message: String,
      code: String,
      exc: Exception
    ) {
      println("Pusher: connection error ${message}")
    }
  }, ConnectionState.ALL)

  return pusher.subscribe(CHANNEL)
}

