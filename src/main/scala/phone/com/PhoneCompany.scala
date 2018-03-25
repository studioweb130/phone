package phone.com

import scala.collection.Iterator
import scala.concurrent.Future

//-- public interface to be consumed by a potential client
trait PhoneCompany {
  def totalCallCostPerCustomer(callLog: Iterator[String]): List[CostByCustomer]
  def readCallLogFile(fileName: String): Future[Iterator[String]]
}

object PhoneContext {
  val TariffSwitchSeconds = 179
  val SecondsHour = 3600
  val SecondsMinute = 60
  val LowTariffPence = 3
  val HighTariffPence = 5
  val timeRegex = """([0-9]{2}:[0-9]{2}:[0-9]{2})""".r
  val phoneRegex = """([0-9]{3}-[0-9]{3}-[0-9]{3})""".r
}

//-- model classes
case class CallLogEntry(customerId: String, phoneNumberCalled: String, durationSeconds: Int, cost: Int)
case class CostByCustomer(customerId: String, cost: Int)

//-- error handling and exceptions
sealed trait PhoneException extends Exception {
  val message: String
  val errorList: List[ValidationFieldError]
}
case class PhoneEntryException(message: String, errorList: List[ValidationFieldError]) extends PhoneException

