package phone.com

import cats.data.Validated.{Invalid, Valid, _}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import phone.com.validation.ValidatableRejection
import scala.util.matching.Regex

trait Validatable[A,T] {
  def validate: Validated[A, T]
}

package object validation {
  type ValidatableRejection[T] = Validatable[ValidationErrorPhone, T]
}

case class ValidationFieldError(field: String, code: String, message: String)
case class ValidationErrorPhone(errorList: List[ValidationFieldError])


trait ValidationUtils {

  val timeRegex = """([0-9]{2}:[0-9]{2}:[0-9]{2})""".r
  val phoneRegex = """([0-9]{3}-[0-9]{3}-[0-9]{3})""".r

  type PhoneValidatableErrorNel[T] = ValidatedNel[ValidationFieldError, T]
  
  implicit class StringValidationConversions(string: String) {
    def asValidatable(field: String, code: String) = new StringValidatable(string, field, code)
  }

  def strip(member: String): String = member.trim

  class StringValidatable(string: String, field: String, code: String) {

    def validateNotEmpty: PhoneValidatableErrorNel[String] = strip(string) match {
      case value if value.isEmpty => invalidNel(ValidationFieldError(field, "NON_EMPTY", s"Cannot be empty"))
      case value => valid(value)
    }

    def validateLength(minLen: Int, maxLen: Int): PhoneValidatableErrorNel[String] = validateNotEmpty.andThen { x =>
      val pattern = "^.{" + minLen + "," + maxLen + "}$".r
      if(maxLen == minLen)
        validateRegex(pattern.r, s"Must be exactly $minLen characters")
      else
        validateRegex(pattern.r, s"Must be between $minLen and $maxLen characters")
    }

    def validateRegex(regex: Regex, errorMessage: String): Validated[NonEmptyList[ValidationFieldError], String] = {
      val strippedMember = strip(string)
      strippedMember match {
        case regex(_*) => valid(strippedMember)
        case _ => invalidNel(ValidationFieldError(field, code, errorMessage))
      }
    }
  }
}

case class CallLog(customerId: String, phoneNumberCalled: String, time: String)  extends ValidatableRejection[CallLog] with ValidationUtils {

  override def validate: Validated[ValidationErrorPhone, CallLog] = {

    val customerIdVal: PhoneValidatableErrorNel[String] = customerId.asValidatable(
      field = "customerId",
      code = "INVALID_CUSTOMERID").validateLength(1, 100)

    val phoneNumberCalledVal: PhoneValidatableErrorNel[String] = phoneNumberCalled.asValidatable(
      field = "phoneNumberCalled",
      code = "INVALID_PHONE").validateRegex(phoneRegex, "Must be a valid phone")

    val timeVal: PhoneValidatableErrorNel[String] = time.asValidatable(
      field = "time",
      code = "INVALID_TIME").validateRegex(timeRegex, "Must be a valid time")

    val validation: PhoneValidatableErrorNel[CallLog] = (customerIdVal |@| phoneNumberCalledVal |@| timeVal) map {
      CallLog.apply(_, _, _)
    }

    validation match {
      case Valid(success) => Valid(success)
      case Invalid(errorList) => Invalid(ValidationErrorPhone(errorList.toList))
    }
  }
}
