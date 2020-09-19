package net.boxmein.tgminecraft

import com.github.kotlintelegrambot.bot
import com.github.kotlintelegrambot.dispatch
import com.github.kotlintelegrambot.dispatcher.command

fun main() {
  println("Hello, mc")

  val apiToken: String = ApiTokenFetcher().apiToken

  assert(apiToken.length > 0)
  assert(System.getenv("APP_ENV") != null)
  assert(System.getenv("AWS_ACCESS_KEY_ID") != null)
  assert(System.getenv("AWS_SECRET_ACCESS_KEY") != null)
  assert(System.getenv("AWS_DEFAULT_REGION") != null)
  assert(System.getenv("SERVER_AWS_INSTANCE_ID") != null)

  val vpsService = VPSService()

  val bot = bot {
    token = apiToken
    dispatch {
      command("start") { bot, update -> 
        bot.sendMessage(
          chatId = update.message!!.chat.id,
          text = "Starting"
        )
        vpsService.startServer()
      }
      command("stop") { bot, update -> 
        bot.sendMessage(
          chatId = update.message!!.chat.id,
          text = "Stopping"
        )
        vpsService.stopServer()
      }
    }
  }

  bot.startPolling()
}
