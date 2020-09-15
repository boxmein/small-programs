plugins {
  application
  kotlin("jvm") version "1.3.21"
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
}

version = "1.0.0"
group = "net.boxmein.tgminecraft"

application {
  mainClassName = "net.boxmein.tgminecraft.MainKt"
}
