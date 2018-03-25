package phone.com

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/*
  NOTES: out of the scope but I thought it´s worth mention some potential improvements
    - Encapsulate the errors with more granularity (contract between services) too small code base
    - Cats EitherT to manage concurrency and error handling, works great with the validations provided
 */

object Main extends App {
  val phoneCompany = PhoneService()
  val readFile_F: Future[Unit] = phoneCompany.readCallLogFile("calls.log").map { lines =>
    val costPerCustomer = phoneCompany.totalCallCostPerCustomer(lines)
    print("********************************TOTAL COST BY CUSTOMER******************************** \n")
    costPerCustomer.map { customerCost =>
      println(s"CUSTOMER[${customerCost.customerId}] - TOTAL COST: ${customerCost.cost}")
    }
    print("********************************END TOTAL COST BY CUSTOMER**************************** ")
  }
  //-- just for testing purposes. Await should always be avoided in prod code
  Await.result(readFile_F, Duration.Inf)
}
