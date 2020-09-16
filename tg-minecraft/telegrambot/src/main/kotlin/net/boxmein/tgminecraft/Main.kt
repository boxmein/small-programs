package net.boxmein.tgminecraft

import com.github.kotlintelegrambot.bot
import com.github.kotlintelegrambot.dispatch
import com.github.kotlintelegrambot.dispatcher.command

fun main() {
  println("Hello, mc")

  val apiToken: String = if (System.getenv("APP_ENV") == "production") {
    ApiTokenFetcher().apiToken
  } else {
    ""
  }

  val bot = bot {
    token = apiToken
    dispatch {
      command("start") { bot, update -> 
        bot.sendMessage(
          chatId = update.message!!.chat.id,
          text = "Starting"
        )
      }
      command("stop") { bot, update -> 
        bot.sendMessage(
          chatId = update.message!!.chat.id,
          text = "Stopping"
        )
      }
    }
  }

  bot.startPolling()
}
