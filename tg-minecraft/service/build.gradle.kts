import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
plugins {
  application
  kotlin("jvm")
  kotlin("plugin.serialization") version "1.3.70"
}

repositories {
  mavenCentral()
}

tasks.withType<Wrapper> {
  gradleVersion = "5.3.1"
  distributionType = Wrapper.DistributionType.ALL
}

dependencies {
  implementation(kotlin("stdlib"))
  implementation("org.jetbrains.kotlinx:kotlinx-serialization-runtime:0.20.0")
  implementation("org.jetbrains.kotlinx:kotlinx-coroutines-core:1.3.9")
  implementation("com.pusher:pusher-java-client:2.2.1")
}

version = "1.0.0"
group = "net.boxmein.minecraftmanager"

application {
  mainClassName = "net.boxmein.minecraftmanager.MainKt"
}

tasks.withType<KotlinCompile> {
  kotlinOptions.jvmTarget = "1.8"
}

