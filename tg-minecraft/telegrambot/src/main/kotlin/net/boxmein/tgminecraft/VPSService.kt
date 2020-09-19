package net.boxmein.tgminecraft

import software.amazon.awssdk.services.ec2.Ec2Client
import software.amazon.awssdk.services.ec2.model.StartInstancesRequest
import software.amazon.awssdk.services.ec2.model.StopInstancesRequest

const val INSTANCE_ID: String = "i-03c2fe6178924d185"

class VPSService {
  val ec2 = Ec2Client.builder().build()
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
