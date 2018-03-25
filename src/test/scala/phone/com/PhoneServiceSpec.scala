package phone.com

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

import scala.collection.Iterator

class PhoneServiceSpec extends FlatSpec with Matchers with PrivateMethodTester with ScalaFutures {

  behavior of "PhoneService"

  val phoneServiceTest = PhoneService()

  val buildCallLogPerLinePrivate = PrivateMethod[PhoneCompany]('buildCallLogPerLine)
  val costCallPerSecondsPrivate = PrivateMethod[PhoneCompany]('costCallPerSeconds)
  val calculateCallCostPrivate = PrivateMethod[PhoneCompany]('calculateCallCost)

  trait PhoneServiceFixtures {
    val logEntry1 = CallLogEntry("A","555-333-212",123,615)
    val logEntry2 = CallLogEntry("A","555-433-242",401,1561)
    val logEntry3 = CallLogEntry("A","555-433-242",63,315)
    val logEntry4 = CallLogEntry("B","555-333-212",80,400)
    val logEntry5 = CallLogEntry("A","555-333-212",70,350)
    val logEntry6 = CallLogEntry("A","555-663-111",129,645)
    val logEntry7 = CallLogEntry("A","555-333-212",268,1162)
    val logEntry8 = CallLogEntry("B","555-334-789",3,15)
    val logEntry9 = CallLogEntry("A","555-663-111",123,615)
    val logEntry10 = CallLogEntry("B","555-334-789",53,265)
    val logEntry11 = CallLogEntry("B","555-971-219",591,2131)
    val logEntry12 = CallLogEntry("B","555-333-212",123,615)
    val logEntry13 = CallLogEntry("B","555-333-212",271,1171)
    val logEntry14 = CallLogEntry("B","555-334-789",119,595)
  }

  it should "thrown PhoneEntryException for when an entry is missing - customer ID" in {
    intercept[PhoneEntryException] {
      phoneServiceTest invokePrivate buildCallLogPerLinePrivate("555-333-212 00:02:03")
    }
  }

  it should "thrown PhoneEntryException for when an entry is missing - phone" in {
    intercept[PhoneEntryException] {
      phoneServiceTest invokePrivate buildCallLogPerLinePrivate("A 00:02:03")
    }
  }

  it should "thrown PhoneEntryException for when an entry is missing - duration" in {
    intercept[PhoneEntryException] {
      phoneServiceTest invokePrivate buildCallLogPerLinePrivate("A 555-333-212")
    }
  }

  it should "thrown PhoneEntryException for when an entry is empty - customer ID" in {
    intercept[PhoneEntryException] {
      phoneServiceTest invokePrivate buildCallLogPerLinePrivate(" 555-333-212 00:02:03")
    }
  }

  it should "thrown PhoneEntryException for when an entry is empty - phone" in {
    intercept[PhoneEntryException] {
      phoneServiceTest invokePrivate buildCallLogPerLinePrivate("A  00:02:03")
    }
  }

  it should "thrown PhoneEntryException for when an entry is empty - duration" in {
    intercept[PhoneEntryException] {
      phoneServiceTest invokePrivate buildCallLogPerLinePrivate("A 555-333-212  a")
    }
  }

  it should "build call log entry successfully" in {
    val expectedResult = CallLogEntry("A","555-333-212",268,1162)
    phoneServiceTest invokePrivate buildCallLogPerLinePrivate("A 555-333-212 00:04:28") shouldBe(expectedResult)
  }

  it should "calculate call cost per seconds - 0" in {
    phoneServiceTest invokePrivate costCallPerSecondsPrivate(0) shouldBe(0)
  }

  it should "calculate call cost per seconds - less than 3 minutes" in {
    phoneServiceTest invokePrivate costCallPerSecondsPrivate(179) shouldBe(895)
  }

  it should "calculate call cost per seconds - 3 minutes" in {
    phoneServiceTest invokePrivate costCallPerSecondsPrivate(180) shouldBe(898)
  }

  it should "calculate call cost per seconds - more than 3 minutes" in {
    phoneServiceTest invokePrivate costCallPerSecondsPrivate(300) shouldBe(1258)
  }

  it should "calculate call cost - empty List" in {
    phoneServiceTest invokePrivate calculateCallCostPrivate(List.empty) shouldBe(List.empty)
  }

  it should "calculate call cost - only one customer - one call - empty list" in {
    val callLogEntry = List(CallLogEntry("A", "555-333-212", 10, 30))
    phoneServiceTest invokePrivate calculateCallCostPrivate(callLogEntry) shouldBe(List.empty)
  }

  it should "calculate call cost - two numbers same customer" in new PhoneServiceFixtures {
    val callLogEntry = List(logEntry11, logEntry12, logEntry4)
    phoneServiceTest invokePrivate calculateCallCostPrivate(callLogEntry) shouldBe(List(CostByCustomer("B",1015)))
  }

  it should "calculate call cost - two numbers same customer - less minutes at higher cost" in new PhoneServiceFixtures {
    val callLogEntry = List(logEntry11, logEntry12, logEntry13, logEntry4)
    phoneServiceTest invokePrivate calculateCallCostPrivate(callLogEntry) shouldBe(List(CostByCustomer("B",2131)))
  }

  it should "calculate call cost - two customers - full data" in new PhoneServiceFixtures {
    val callLogEntry = List(logEntry1, logEntry2, logEntry3, logEntry4, logEntry5, logEntry6, logEntry7, logEntry8,
      logEntry9, logEntry10, logEntry11, logEntry12, logEntry13, logEntry14)
    val expectedResult = List(CostByCustomer("A",5263), CostByCustomer("B",3006))
    phoneServiceTest invokePrivate calculateCallCostPrivate(callLogEntry) shouldBe(expectedResult)
  }

  it should "read file" in new PhoneServiceFixtures {
    whenReady(phoneServiceTest.readCallLogFile("calls.log")) { result :Iterator[String] =>
      result.size shouldBe(15)
    }
  }

}
