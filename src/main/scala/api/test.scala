package api

import cats.Monad
import cats.effect._
import cats.implicits._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

object test extends IOApp {

  case class Patients(ID: Int, name: String, age: Int, location: String, email: String, password: String)
  case class Specialist(ID: Int, name: String, category: Int, location: String, email: String, availability: Int)
  case class Type(ID: Int, name: String)
  case class Symptoms(ID: Int, name: String, description: String)
  case class Illness(ID: Int, name: String, description: String)
  case class SubIllness(ID: Int, name: String, illness: Int)
  case class IllnessSymptoms(ID: Int, illness: Int, symptom: Int)
  case class Bill(ID: Int, patient: Int, amount: Float)

  // Patients query matcher used to make search more user-friendly
  private object PatientLocationQueryParamMatcher extends QueryParamDecoderMatcher[String]("location")
  private object PatientEmailQueryParameterMatcher extends QueryParamDecoderMatcher[String]("email")

  // Specialist query matchers
  private object SpecialistLocationQueryMatcher extends QueryParamDecoderMatcher[String]("location")
  private object SpecialistCategoryQueryMatcher extends QueryParamDecoderMatcher[Int]("category")
  private object SpecialistAvailabilityMatcher extends QueryParamDecoderMatcher[Int]("availability")

  // Define routes
  private def patientsRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "patients" :? PatientLocationQueryParamMatcher(location) +& PatientEmailQueryParameterMatcher(email) =>
        Ok(s"Searching for patients with location: $location and email: $email")
    }
  }

  private def specialistRoutes[F[_]: Monad]: HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {
      case GET -> Root / "specialists" :? SpecialistCategoryQueryMatcher(category) +& SpecialistAvailabilityMatcher(availability) +& SpecialistLocationQueryMatcher(location) =>
        Ok(s"Searching for specialists with category: $category, availability: $availability, and location: $location")
    }
  }

  // Combine the routes
  def allRoutes[F[_]: Monad]: Http[F] = patientsRoutes[F] <+> specialistRoutes[F]

  def allRoutesComplete[F[_]: Monad] : HttpApp[F] = allRoutes[F]
  override def run(args: List[String]): IO[ExitCode] = {
    // Wire different routes to different paths
    val api = Router(
      "/api" -> patientsRoutes[IO],
      "/api/admin" -> specialistRoutes[IO]
    )
    BlazeServerBuilder[IO](runtime.compute)
      .bindHttp(8080, "localhost")
      .withHttpApp(allRoutesComplete)
      .resource
      .use(_ => IO.never)
      .as(ExitCode.Success)
  }
}
