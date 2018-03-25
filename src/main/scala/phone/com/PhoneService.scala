package phone.com

import cats.data.Validated.{Invalid, Valid}

import scala.collection.Iterator
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.io.Source

class PhoneService extends PhoneCompany {

  import PhoneContext._

  def readCallLogFile(fileName: String): Future[Iterator[String]] = Future {
    blocking {
      Source.fromResource(fileName).getLines
    }
  }

  def totalCallCostPerCustomer(callLog: Iterator[String]): List[CostByCustomer] = {
    //-- validate and build model. Important to calculate the cost of each individual call to avoid miscalculations later
    val callLogEntries: List[CallLogEntry] = callLog.filter(!_.isEmpty).map { line =>
      buildCallLogPerLine(line)
    }.toList

    calculateCallCost(callLogEntries)
  }

  private def costCallPerSeconds(seconds: Int): Int = {
    if(seconds <= TariffSwitchSeconds)
      seconds * HighTariffPence
    else {
      val outstanding = seconds - TariffSwitchSeconds
      (TariffSwitchSeconds * HighTariffPence) + (outstanding * LowTariffPence)
    }
  }

  private def buildCallLogPerLine(callLog: String): CallLogEntry = {
    val callLogEntry: Array[String] = callLog.split(" ")
    if(callLogEntry.size < 3)
      throw new  PhoneEntryException("Invalid entry. Missing entry in the call log.", List.empty)
    else {
        CallLog(callLogEntry(0), callLogEntry(1), callLogEntry(2)).validate match {
          case Valid(callLogEntity) => {
            //-- at this point we can be 100% sure the data is correct
            val duration = callLogEntity.time.split(":")
            val seconds = ((duration(0).toInt * SecondsHour) + (duration(1).toInt * SecondsMinute) + (duration(2).toInt))
            CallLogEntry(callLogEntity.customerId, callLogEntity.phoneNumberCalled, seconds, costCallPerSeconds(seconds))
          }
          case Invalid(rejection) => {
            throw new PhoneEntryException(s"Invalid entry. Empty record for a call log entry.", rejection.errorList)
          }
      }
    }
  }

  private def calculateCallCost(calls: List[CallLogEntry]): List[CostByCustomer] = {
    if(calls.isEmpty) List.empty[CostByCustomer] else {
      //-- totalCallCostPerCustomer [Map(CustomerID - Phone) - totalCost]
      val totalCallCostPerCustomer: Map[(String, String), Int] = calls.groupBy(record =>
        (record.customerId, record.phoneNumberCalled )).map(customerPhoneListCalls =>
        (customerPhoneListCalls._1, (customerPhoneListCalls._2.map(_.cost).sum)))

      if(totalCallCostPerCustomer.size == 1) List.empty[CostByCustomer] else {
        //-- ((String, String), Int) - (CustomerID - Phone) - totalCost
        val promotion = totalCallCostPerCustomer.toList.sortBy(_._2).last
        //-- remove the promotion (customer-phone) from the list and re-group again by customer ID and total cost = desired return type
        totalCallCostPerCustomer.filterNot(_._1 == promotion._1).groupBy(_._1._1).map(customerPhoneListCalls =>
          CostByCustomer(customerPhoneListCalls._1, customerPhoneListCalls._2.map(_._2).sum)).toList
      }
    }
  }
}

object PhoneService {
  def apply() = {
    new PhoneService()
  }
}

