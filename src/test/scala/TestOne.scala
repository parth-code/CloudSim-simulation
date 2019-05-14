import org.cloudbus.cloudsim.{CloudletSchedulerSpaceShared, Vm}
import org.scalatest.FlatSpec

class TestOne extends FlatSpec{
  val v1 = new Vm(1, 1, 100, 1, 100, 10, 100, "vmm", new CloudletSchedulerSpaceShared)
  val v2 = new Vm(1, 1, 100, 1, 100, 10, 100, "vmm", new CloudletSchedulerSpaceShared)
  val l = List(v1, v2)
  val a = new java.util.ArrayList[Vm]
  l.map(a.add(_))
  assert(a.size()!=2)
}
