package be.kuleuven.proman

import java.io.File

import fs2.Task
import fs2.interop.cats._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl._
import org.http4s.server.blaze.BlazeBuilder

import scala.io.StdIn
import scalatags.Text.all._

object ProManApp extends App {

  // Step 1: Create a list of Projects, other extensions are to change the case class
  // itself and expand upon its current definition.
  var projects: Seq[Project] = Seq(Project("project"))

  def serveFrag(tag: Frag) = Ok(tag.render).withType(MediaType.`text/html`)

  val ui = new ProjectTemplate(scalatags.Text)

  // Step 2: Define an Http4s HttpService
  val helloWorld: HttpService = HttpService {
    case GET -> Root / "hello" => helloWorldHtml
    case GET -> Root / "projects" =>
      serveFrag(
        html(
          head(),
          body(
            h1("Projects"),
            ui.projectsTemplate(projects)
          )
        )
      )

    case GET -> Root / "jsprogram" => javascriptProgramResponse

    case GET -> Root / "service" / "project" => Ok(projects.asJson)

    case req @ POST -> Root / "service" / "project" =>
      for {
        project <- req.as(jsonOf[Project])
        response <- Ok(project.name.asJson)
      } yield {
        projects = projects :+ project
        println(projects)
        response
      }

    case GET -> Root / "jsprojects" =>
      serveFrag(
        html(
          head(),
          body(h1("Projects"),
               div(id := "content"),
               script(src := "/jsprogram"))
        )
      )
  }

  // Step 2 HELPER: Use this Task to define a hello world page.
  // Step 3: Expand this page to render a proper Html page that displays
  // all the projects you defined in Step 1. Be smart about this and write code
  // that does not need modification when the list changes size...
  lazy val helloWorldHtml: Task[Response] =
    serveFrag(h1("You'll complete me!").render)

  // Standard server that binds to localhost:8080/
  val server =
    BlazeBuilder.bindHttp(8080, "localhost").mountService(helloWorld, "/").run

  // Block on reading a line, makes it easy to stop in your terminal (hit enter).
  StdIn.readLine()
  server.shutdownNow()

  // Step 4 HELPER: Extend your Http4s HttpService to serve the compilation result of
  // the Scala.js program (your js code). You can use this task to get started.
  lazy val javascriptProgramResponse: Task[Response] =
    StaticFile
      .fromFile(new File("./js/target/scala-2.11/js-fastopt.js"))
      .getOrElseF(NotFound())
}
