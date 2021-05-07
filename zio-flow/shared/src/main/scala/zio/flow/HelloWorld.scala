package zio.flow

import java.net.URI

import zio.console._
import zio.flow.PolicyRenewalExample.{ Email, Policy, PolicyId, PropertyAddress }
import zio.flow.ZFlow.Input
import zio.flow.ZFlowExecutor.InMemory
import zio.schema._
import zio.{ App, ExitCode, Has, IO, Task, UIO, URIO, ZIO }

object HelloWorld extends App {

  override def run(args: List[String]): URIO[Console, ExitCode] =
    executeZFlow.exitCode

  case class Policy(id: PolicyId, address: PropertyAddress, userEmail: Email, evaluatorEmail: Email)

  val opEx: OperationExecutor[Console] = new OperationExecutor[Console] {

    override def execute[I, A](input: I, operation: Operation[I, A]): ZIO[Console, ActivityError, A] =
      operation match {
        case Operation.Http(url, method, headers, inputSchema, outputSchema) =>
          putStrLn("Make http request to " + url) *> ZIO.succeed(().asInstanceOf[A])
        case Operation.SendEmail(server, port)                               => putStrLn("Send email") *> ZIO.succeed(().asInstanceOf[A])
      }
  }

  val exampleZFlow: ZFlow[Any, ActivityError, String] = for {
    s       <- policyClaimStatus("Test String")
    exZFlow <- ZFlow.succeed(s)
  } yield exZFlow

  def policyClaimStatus: Activity[String, String] =
    Activity[String, String](
      "get-policy-claim-status",
      "Returns whether or not claim was made on a policy for a certain year",
      Operation.Http[String, String](
        URI.create("http://www.checkPolicyClaim.com"),
        "GET",
        Map.empty[String, String],
        implicitly[Schema[String]],
        implicitly[Schema[String]]
      ),
      exampleCheckZFlow,
      exampleCompensateZflow
    )

  def exampleCheckZFlow: ZFlow[String, ActivityError, String]   = Input[String](implicitly[Schema[String]])
  val exampleCompensateZflow: ZFlow[String, ActivityError, Any] = Input[String](implicitly[Schema[String]])

  val executeZFlow: IO[ActivityError, String] =
    InMemory(Has(zio.clock.Clock.Service.live) ++ Has(zio.console.Console.Service.live), opEx)
      .submit("Unique", exampleZFlow)
  //.flatMap(x => putStrLn(x.toString))

  //  val myAppLogic: ZIO[Console, IOException, Unit] =
  //    for {
  //      _    <- putStrLn("Hello! What is your name?")
  //      name <- getStrLn
  //      _    <- putStrLn(s"Hello, $name, welcome to ZIO!")
  //    } yield ()
}
