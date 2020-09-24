package net.boxmein.tgminecraft

import software.amazon.awssdk.services.ec2.Ec2Client
import software.amazon.awssdk.services.ec2.model.StartInstancesRequest
import software.amazon.awssdk.services.ec2.model.StopInstancesRequest
import software.amazon.awssdk.regions.Region

val INSTANCE_ID: String? = System.getenv("SERVER_AWS_INSTANCE_ID")
val REGION: String = System.getenv("AWS_DEFAULT_REGION") ?: "eu-north-1"

class VPSService {
  val ec2 = Ec2Client.builder()
    .region(Region.of(REGION))
    .build()

  fun startServer() {
    val req = StartInstancesRequest
      .builder()
      .instanceIds(INSTANCE_ID)
      .build()

    ec2.startInstances(req)

    println("Start instance request sent")
  }

  fun stopServer() {
    val req = StopInstancesRequest.builder()
      .instanceIds(INSTANCE_ID)
      .build()

    ec2.stopInstances(req)

    println("Stop instance request sent")
  }
}
