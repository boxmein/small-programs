package net.boxmein.minecraftmanager

import java.lang.ProcessBuilder.Redirect
import java.io.File
import java.io.PrintWriter
import java.io.OutputStreamWriter
import java.util.concurrent.TimeUnit

class MinecraftManager {

  val SERVER_PATH: String = System.getenv("MINECRAFT_SERVER_PATH") ?: "./server.jar"
  val SERVER_WORKDIR: String = System.getenv("WORKING_DIRECTORY") ?: "/home/ubuntu/minecraft"

  var process: Process? = null

  fun start() {
    println("Java: ${System.getProperty("java.home")}/bin/java")
    println("JAR: ${SERVER_PATH}")
    println("Work dir: ${SERVER_WORKDIR}")
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
    
    if (process == null) {
      throw RuntimeException("Process did not create")
    }

    process?.waitFor()
    println("Process exited ${process?.exitValue()}")
  }

  fun saveAll() {
    sendConsoleCommand("save-all")
  }

  fun stop() {
    val proc = process
    if (proc == null) {
      println("Process no existo")
      return
    }
    println("Stopping")
    sendConsoleCommand("stop")
    if (!proc.waitFor(60, TimeUnit.SECONDS)) {
      println("Stopping forcefully")
      proc.destroy()
    }
  }

  fun sendConsoleCommand(command: String) {
    val proc = process
    if (proc == null) {
      println("Process no existo")
      return
    }
    println("Command: ${command}")

    proc.outputWriter.use { writer ->
      writer.write(command + "\n")
      writer.flush()
    }
  }
}

val Process.outputWriter: PrintWriter
  get() = PrintWriter(OutputStreamWriter(this.outputStream))
