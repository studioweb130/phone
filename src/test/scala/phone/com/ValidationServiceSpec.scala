package phone.com

import org.scalatest.{FlatSpec, Matchers}
import cats.data.Validated.{Invalid, Valid}

class ValidationServiceSpec extends FlatSpec with Matchers {

  behavior of "CallLog"

  it should "create an entity CallLog - successfully" in {
    val callLog = CallLog("A", "555-333-212", "00:02:03")
    callLog.validate match {
      case Valid(success) => success shouldBe(callLog)
      case Invalid(rejection) => fail()
    }
  }

  it should "create an entity CallLog - invalid customerID" in {
    val callLog = CallLog("", "555-333-212", "00:02:03")
    callLog.validate match {
      case Valid(success) => fail()
      case Invalid(rejection) => {
        rejection.errorList.size shouldBe(1)
        rejection.errorList.head shouldBe(ValidationFieldError("customerId","NON_EMPTY","Cannot be empty"))
      }
    }
  }

  it should "create an entity CallLog - invalid phone number - empty" in {
    val callLog = CallLog("A", "", "00:02:03")
    callLog.validate match {
      case Valid(success) => fail()
      case Invalid(rejection) => {
        rejection.errorList.size shouldBe(1)
        rejection.errorList.head shouldBe(ValidationFieldError("phoneNumberCalled","INVALID_PHONE","Must be a valid phone"))
      }
    }
  }

  it should "create an entity CallLog - invalid phone number - format" in {
    val callLog = CallLog("A", "555-1a2-212", "00:02:03")
    callLog.validate match {
      case Valid(success) => fail()
      case Invalid(rejection) => {
        rejection.errorList.size shouldBe(1)
        rejection.errorList.head shouldBe(ValidationFieldError("phoneNumberCalled","INVALID_PHONE","Must be a valid phone"))
      }
    }
  }

  it should "create an entity CallLog - invalid time - empty" in {
    val callLog = CallLog("A", "555-333-212", "")
    callLog.validate match {
      case Valid(success) => fail()
      case Invalid(rejection) => {
        rejection.errorList.size shouldBe(1)
        rejection.errorList.head shouldBe(ValidationFieldError("time","INVALID_TIME","Must be a valid time"))
      }
    }
  }

  it should "create an entity CallLog - invalid time - format" in {
    val callLog = CallLog("A", "555-333-212", "0a:02:03")
    callLog.validate match {
      case Valid(success) => fail()
      case Invalid(rejection) => {
        rejection.errorList.size shouldBe(1)
        rejection.errorList.head shouldBe(ValidationFieldError("time","INVALID_TIME","Must be a valid time"))
      }
    }
  }

  it should "create an entity CallLog - invalid time and phone" in {
    val callLog = CallLog("A", "555-3a3-212", "0a:02:03")
    callLog.validate match {
      case Valid(success) => fail()
      case Invalid(rejection) => {
        rejection.errorList.size shouldBe(2)
      }
    }
  }

  it should "create an entity CallLog - whole elements invalid value" in {
    val callLog = CallLog("", "555-3a3-212", "0a:02:03")
    callLog.validate match {
      case Valid(success) => fail()
      case Invalid(rejection) => {
        rejection.errorList.size shouldBe(3)
      }
    }
  }
}
