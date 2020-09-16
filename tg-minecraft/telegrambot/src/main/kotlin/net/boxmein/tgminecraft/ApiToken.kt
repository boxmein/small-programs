package net.boxmein.tgminecraft

import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.secretsmanager.SecretsManagerClient
import software.amazon.awssdk.services.secretsmanager.model.GetSecretValueRequest
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonConfiguration
import kotlinx.serialization.json.content

const val secret = "tgminebot/auth"

class ApiTokenFetcher {
  val apiToken: String = System.getenv("TELEGRAM_BOT_TOKEN")

  /*
  AWS Secrets Manager
  init {
    val client = SecretsManagerClient.builder().region(Region.EU_NORTH_1).build()
    val secret = client.getSecretValue(GetSecretValueRequest.builder().secretId(secret).build())
    val parser = Json(JsonConfiguration.Default)
    val result = parser.parseJson(secret.secretString()).jsonObject
    apiToken = result["apiToken"]!!.content
  }
  */
}
