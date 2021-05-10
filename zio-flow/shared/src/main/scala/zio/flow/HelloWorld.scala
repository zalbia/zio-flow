package zio.flow

import java.net.URI
import zio.console._
import zio.flow.ZFlow.Input
import zio.flow.ZFlowExecutor.InMemory
import zio.schema._
import zio.{App, ExitCode, Has, IO, URIO, ZIO}

import scala.reflect.ClassTag

object HelloWorld extends App {

  override def run(args: List[String]): URIO[Console, ExitCode] =
    executeZFlow.flatMap(x => putStrLn(if(x) "true" else "false")).exitCode

  val opEx: OperationExecutor[Console] = new OperationExecutor[Console] {

    override def execute[I, A](input: I, operation: Operation[I, A]): ZIO[Console, ActivityError, A] =
      operation match {
        case Operation.Http(url, method, headers, inputSchema, outputSchema) =>
          putStrLn("Make http request to " + url) *> ZIO.succeed(().asInstanceOf[A])
        case Operation.SendEmail(server, port)                               => putStrLn("Send email") *> ZIO.succeed(().asInstanceOf[A])
      }
  }

  def opEx1[T](mockResponse: T): OperationExecutor[Console] = new OperationExecutor[Console] {

    override def execute[I, A](input: I, operation: Operation[I, A]): ZIO[Console, ActivityError, A] =
      operation match {
        case Operation.Http(url, method, headers, inputSchema, outputSchema) =>
          putStrLn("Make http request to " + url) *> ZIO.succeed(mockResponse.asInstanceOf[A])
        case Operation.SendEmail(server, port)                               => putStrLn("Send email") *> ZIO.succeed(().asInstanceOf[A])
      }
  }

  case class PolicyEx(id: String, userEmail: String, evaluatorEmail: String)
  implicit val policySchema: Schema[PolicyEx] = DeriveSchema.gen[PolicyEx]

  val testPolicy: PolicyEx = PolicyEx("abcd", "efgh", "akjhd")

  val exampleZFlow: ZFlow[Any, ActivityError, Boolean] = for {
    s       <- policyClaimStatus(testPolicy)
    exZFlow <- ZFlow.succeed(s)
  } yield exZFlow

  def policyClaimStatus: Activity[PolicyEx, Boolean] =
    Activity[PolicyEx, Boolean](
      "get-policy-claim-status",
      "Returns whether or not claim was made on a policy for a certain year",
      Operation.Http[PolicyEx, Boolean](
        URI.create("http://www.checkPolicyClaim.com"),
        "GET",
        Map.empty[String, String],
        implicitly[Schema[PolicyEx]],
        implicitly[Schema[Boolean]]
      ),
      exampleCheckZFlow,
      exampleCompensateZflow
    )

  def exampleCheckZFlow: ZFlow[PolicyEx, ActivityError, Boolean] =
    Input[PolicyEx](implicitly[Schema[PolicyEx]]).as(Remote(true))
  val exampleCompensateZflow: ZFlow[Boolean, ActivityError, Any] = Input[Boolean](implicitly[Schema[Boolean]])

  val executeZFlow: IO[ActivityError, Boolean] =
    InMemory(Has(zio.clock.Clock.Service.live) ++ Has(zio.console.Console.Service.live), opEx1(true))
      .submit("Unique", exampleZFlow)

  //.flatMap(x => putStrLn(x.toString))

  //  val myAppLogic: ZIO[Console, IOException, Unit] =
  //    for {
  //      _    <- putStrLn("Hello! What is your name?")
  //      name <- getStrLn
  //      _    <- putStrLn(s"Hello, $name, welcome to ZIO!")
  //    } yield ()
}
