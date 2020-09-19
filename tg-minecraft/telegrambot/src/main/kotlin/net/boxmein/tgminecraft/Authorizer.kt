package net.boxmein.tgminecraft

import com.github.kotlintelegrambot.entities.User

class Authorizer {
  fun authorizeUser(user: User?): Boolean {
    if (user == null) {
      return false
    }
    // 29399932L = boxmein

    return user.id == 29399932L
  }
}
