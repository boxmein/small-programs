import org.jetbrains.kotlin.gradle.tasks.KotlinCompile
plugins {
  application
  kotlin("jvm") version "1.3.21"
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
}

version = "1.0.0"
group = "net.boxmein.minecraftmanager"

application {
  mainClassName = "net.boxmein.minecraftmanager.MainKt"
}

tasks.withType<KotlinCompile> {
  kotlinOptions.jvmTarget = "1.8"
}

