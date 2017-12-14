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
  val project = Project("project1")
  var entry1 = Entry(1, done=true, "entry 1")
  var entry2 = Entry(2, done=false, "entry 2")
  project.entries = Seq(entry1, entry2)
  var projects: Seq[Project] = Seq(project)

  def getProject(name: String): Project = projects.filter(_.hasName(name)).head

  def hasName(name: String)(p: Project): Boolean = p.name == name
  def hasId(id: Int)(e: Entry): Boolean = e.id == id

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

    case GET -> Root / "service" / "project" / projectName =>
      val project: Project = projects.filter(hasName(projectName)).head
      Ok(project.entries.asJson)

    case req @ POST -> Root / "service" / "project" =>
      for {
        project <- req.as(jsonOf[Project])
        response <- Ok(project.name.asJson)
      } yield {
        projects = projects :+ project
        println(projects)
        response
      }

    case req @ POST -> Root / "service" / "project" / projectName / "add" / entryContent =>
      val project = getProject(projectName)
      val entry = Entry(project.entries.size+1, done = false, entryContent)
      project.addEntry(entry)
      Ok(entry.id.asJson)

    case req @ POST -> Root / "service" / "project" / projectName / entryId / "markasdone" =>
      val project = getProject(projectName)
      val entry = project.getEntry(entryId.toInt)
      entry.done = true
      Ok(entry.asJson)

    case req @ POST -> Root / "service" / "project" / projectName / entryId / "markasundone" =>
      val project = getProject(projectName)
      val entry = project.getEntry(entryId.toInt)
      entry.done = false
      Ok(entry.asJson)

    case req @ POST -> Root / "service" / "project" / projectName / entryId / "changecontent" / entryContent =>
      val project = getProject(projectName)
      val entry = project.getEntry(entryId.toInt)
      entry.content = entryContent
      Ok(entry.asJson)

    case GET -> Root / "jsprojects" =>
      serveFrag(
        html(
          head(),
          body(fontFamily :="Arial, Helvetica, sans-serif;")(
            h1("Projects",
              textAlign := "center",
              textTransform := "uppercase"),
            div(id := "content"),
            br(),
            div(id := "warningBox",
              textAlign := "center"),
            script(src := "/jsprogram"))
        )
      )
  }

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
