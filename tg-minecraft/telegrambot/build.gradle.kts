import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

val dockerRepo: String by project

plugins {
  application
  kotlin("jvm") version "1.3.21"
  kotlin("plugin.serialization") version "1.3.70"
  id("com.palantir.docker") version "0.25.0"
}

repositories {
  mavenCentral()
  maven {
    url = uri("https://jitpack.io")
  }
}

tasks.withType<Wrapper> {
  gradleVersion = "5.3.1"
  distributionType = Wrapper.DistributionType.ALL
}

dependencies {
  implementation(kotlin("stdlib"))
  implementation("io.github.kotlin-telegram-bot.kotlin-telegram-bot:telegram:5.0.0")
  implementation("software.amazon.awssdk:secretsmanager:2.14.21")
  implementation("software.amazon.awssdk:ec2:2.14.21")
  implementation("org.jetbrains.kotlinx:kotlinx-serialization-runtime:0.20.0")
}

version = "1.0.0"
group = "net.boxmein.tgminecraft"

application {
  mainClassName = "net.boxmein.tgminecraft.MainKt"
}

tasks.withType<KotlinCompile> {
  kotlinOptions.jvmTarget = "1.8"
}

docker {
  name = "${dockerRepo}/boxmein-tgminecraft-bot"
  files(tasks.getByName("distZip").outputs)
}
