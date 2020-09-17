package net.boxmein.minecraftmanager

import java.lang.ProcessBuilder.Redirect
import java.io.File

class MinecraftManager {

  val SERVER_PATH: String = System.getenv("MINECRAFT_SERVER_PATH") ?: "server.jar"
  val SERVER_WORKDIR: String = System.getenv("WORKING_DIRECTORY") ?: "/home/ubuntu/minecraft"

  var process: Process? = null

  fun start() {
    process = 
      ProcessBuilder(
        "${System.getProperty("java.home")}/bin/java",
        "-Xmx2048M",
        "-Xms2048M",
        "-jar",
        SERVER_PATH)
      .directory(File(SERVER_WORKDIR))
      .redirectOutput(Redirect.INHERIT)
      .start()
  }

  fun stop() {
    sendConsoleCommand("save-all")
    sendConsoleCommand("stop")
    Thread.sleep(20 * 1000)
    process?.destroy()
  }

  fun sendConsoleCommand(command: String) {
    process?.outputStream?.write((command + "\n").toByteArray(Charsets.UTF_8))
  }
}
