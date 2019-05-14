/*
  This is a IaaS model of a big data computation task, where each vm is on a separate host

*/
import java.text.DecimalFormat
import java.util.Calendar

import com.typesafe.config.ConfigFactory
import org.cloudbus.cloudsim._
import org.cloudbus.cloudsim.core.CloudSim
import org.cloudbus.cloudsim.provisioners.{BwProvisionerSimple, PeProvisionerSimple, RamProvisionerSimple}
import org.slf4j.LoggerFactory

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}
object ModelOne {
  val logger = LoggerFactory.getLogger("Test")
  //Typesafe config
  val config = ConfigFactory.load()
  config.checkValid(ConfigFactory.defaultReference(), "Test")

  def main (args: Array[String]): Unit ={
    logger.debug("Starting Model 1")
    try{

      // First step: Initialize the CloudSim package. It should be called
      // before creating any entities.
      val num = config.getInt("num_user")
      val calendar = Calendar.getInstance()
      val trace_flag = config.getBoolean("trace_flag")

      // Initialize the CloudSim library
      CloudSim.init(num, calendar, trace_flag)

      // Second step: Create Datacenters
      //Datacenters are the resource providers in CloudSim. We need at list one of them to run a CloudSim simulation
      val (datacenter0, datacenter1) = createDatacenters()

      //Third step: Create Broker
      val broker_name = config.getString("broker.name")
      val broker = createBroker(broker_name).get
      val brokerId = broker.getId

      //Fourth step: Create VMs and Cloudlets
      val vmList = toJList(createVM(brokerId))

      val cloudletList = toJList(createCloudlet(brokerId))

      //submit vm list to the broker
      broker.submitVmList(vmList)

      //submit cloudlet list to the broker
      broker.submitCloudletList(cloudletList)

      // Fifth step: Starts the simulation
      CloudSim.startSimulation

      // Final step: Print results when simulation is over
      val newList: List[Cloudlet] = toSList(broker.getCloudletReceivedList)
      //newList.addAll(globalBroker.getBroker.getCloudletReceivedList)

      CloudSim.stopSimulation()

      //Log.print("=============> User " + broker + "    ")
      printCloudletList(newList)

      logger.debug("Model finished!")
    }
    catch{
      case e: Exception => e.printStackTrace()
      logger.error("The simulation has been terminated due to an unexpected error")
    }
  }
  private def createDatacenters(): Tuple2[Datacenter, Datacenter] = {
    val mips_1 = config.getInt("host.one.mips")
    val cores_1 = config.getInt("host.one.cores")
    val pe_temp_list_1 = for( i<- 0 to cores_1 ) yield new Pe(i, new PeProvisionerSimple(mips_1))
    val pe_list_1 = toJList(pe_temp_list_1.toList)

    val host1 = new Host(
      config.getInt("host.one.hostID"),
      new RamProvisionerSimple(config.getInt("host.one.ram")),
      new BwProvisionerSimple(config.getInt("host.one.bw")),
      config.getInt("host.one.storage"),
      pe_list_1,
      new VmSchedulerSpaceShared(pe_list_1)
    )
    val mips_2 = config.getInt("host.two.mips")
    val cores_2 = config.getInt("host.two.cores")
    val pe_temp_list_2 = for( i<- 0 to cores_2 )
      yield new Pe(i, new PeProvisionerSimple(mips_2))
    val pe_list_2 = toJList(pe_temp_list_2.toList)

    val host2 = new Host(
      config.getInt("host.two.hostID"),
      new RamProvisionerSimple(config.getInt("host.two.ram")),
      new BwProvisionerSimple(config.getInt("host.two.bw")),
      config.getInt("host.two.storage"),
      pe_list_2,
      new VmSchedulerSpaceShared(pe_list_2)
    )

    val host_list_1 = toJList(List(host1))
    val host_list_2 = toJList(List(host2))

    val characteristics1 = new DatacenterCharacteristics(
      config.getString("datacenter.one.arch"),
      config.getString("datacenter.one.os"),
      config.getString("datacenter.one.vmm"),
      host_list_1,
      config.getDouble("datacenter.one.timezone"),
      config.getDouble("datacenter.one.cost"),
      config.getDouble("datacenter.one.costPerMem"),
      config.getDouble("datacenter.one.costPerStorage"),
      config.getDouble("datacenter.one.costPerBw")
    )
    val storageList1 = new java.util.LinkedList[Storage]
    val datacenter1 = new Datacenter(
      config.getString("datacenter.one.name"),
      characteristics1,
      new VmAllocationPolicySimple(host_list_1),
      storageList1,
      0
    )
    logger.debug("Datacenter 1 created successfully")
    val characteristics2 = new DatacenterCharacteristics(
      config.getString("datacenter.two.arch"),
      config.getString("datacenter.two.os"),
      config.getString("datacenter.two.vmm"),
      host_list_2,
      config.getDouble("datacenter.two.timezone"),
      config.getDouble("datacenter.two.cost"),
      config.getDouble("datacenter.two.costPerMem"),
      config.getDouble("datacenter.two.costPerStorage"),
      config.getDouble("datacenter.two.costPerBw")
    )
    val storageList2 = new java.util.LinkedList[Storage]

    val datacenter2 = new Datacenter(
      config.getString("datacenter.two.name"),
      characteristics2,
      new VmAllocationPolicySimple(host_list_2),
      storageList2,
      0
    )
    logger.debug("Datacenter 1 created successfully")

    (datacenter1, datacenter2)
  }


  private def createBroker(broker_name: String):Try[DatacenterBroker] = {
    val broker = Try(new DatacenterBroker(broker_name))
    broker match {
      case Success(v) =>
        logger.info("Broker created successfully")
      case Failure(e) =>
        logger.error("Broker creation failure")
        logger.error("Info from the exception: " + e.getMessage)
    }
    broker
  }

  private def createVM(brokerId : Int): List[Vm] = {
    val vmList: List[Vm] =
      (0 to config.getInt("vm.numbers")).foldLeft(List.empty[Vm]){
        (vmList, id: Int) => new Vm(
          id+1,
          brokerId,
          config.getInt("vm.mips"),
          config.getInt("vm.pesNumber"),  //number of cpus
          config.getInt("vm.ram"),  //vm memory (MB)
          config.getLong("vm.bw"),
          config.getLong("vm.size"), //image size (MB)
          config.getString("vm.vmm"), //VMM name
          new CloudletSchedulerSpaceShared()) :: vmList
      }
    logger.info("Vm generation complete")
    vmList
  }

  private def createCloudlet(brokerId: Int): List[Cloudlet]= {
    val utilizationModel = new UtilizationModelFull
    val cloudletList: List[Cloudlet] =
      (0 to config.getInt("cloudlet.numbers")).foldLeft(List.empty[Cloudlet]){
        (cloudletList, id: Int) => new Cloudlet(
          id+1,
          config.getLong("cloudlet.length"),
          config.getInt("cloudlet.pesNumber"),
          config.getLong("cloudlet.fileSize"),
          config.getLong("cloudlet.outputSize"),
          utilizationModel,
          utilizationModel,
          utilizationModel) :: cloudletList
    }
    logger.info("Cloudlet generation complete")
    cloudletList.foreach(cloudlet => cloudlet.setUserId(brokerId))
    cloudletList
  }

  private def printCloudletList(cloudList: List[Cloudlet]): Unit ={
    val indent = "    "
    Log.printLine()
    Log.printLine("========== OUTPUT ==========")
    Log.printLine("Cloudlet ID" + indent + "STATUS" + indent +
                  "Data center ID" + indent + "VM ID" + indent +
                  indent + "Time" + indent + "Start Time" + indent +
                  "Finish Time"+ indent + indent + "Time taken" + indent +
                  "Cost")
    val dft = new DecimalFormat("###.##")
    cloudList.foreach(cloudlet =>{
      Log.print(indent + cloudlet.getCloudletId + indent + indent)
      if (cloudlet.getCloudletStatus  == Cloudlet.SUCCESS) {
        Log.printLine("SUCCESS")
        Log.printLine(indent + indent + cloudlet.getResourceId() +
          indent + indent + indent + indent + cloudlet.getVmId() +
          indent + indent + indent + dft.format(cloudlet.getActualCPUTime()) +
          indent + indent + dft.format(cloudlet.getExecStartTime()) +
          indent + indent + indent + dft.format(cloudlet.getFinishTime) +
          indent + indent + indent + dft.format((cloudlet.getFinishTime - cloudlet.getExecStartTime)) +
          indent + indent + indent + dft.format(costPerCloudLet(cloudlet)))
      }
    })
  }

  private def costPerCloudLet(cloudlet: Cloudlet) = {
    val cloudletExecutionTime = cloudlet.getFinishTime - cloudlet.getExecStartTime
    val resArray = cloudlet.getAllResourceId
    val costPerSec:Double = resArray.foldLeft(0.0){
      (result: Double, resource) =>
        result.toDouble + (cloudlet.getCostPerSec(resource) * cloudlet.getActualCPUTime(resource))
    }
    costPerSec
  }
  //Method to convert to ArrayList
  def toJList[T](l:List[T]):java.util.List[T] = {
    val a = new java.util.ArrayList[T]
    l.map(a.add(_))
    a
  }

  def toSList[T](l: java.util.List[T]): List[T] = {
    var a = ListBuffer[T]()
    for (r <- 0 until l.size) a += l.get(r)
    a.toList
  }
}