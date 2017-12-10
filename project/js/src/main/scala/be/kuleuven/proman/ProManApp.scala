package be.kuleuven.proman

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw.{Event, HTMLButtonElement, HTMLInputElement, NodeList}

import scalatags.Text.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object ProManApp {

  def main(args: Array[String]): Unit = {

    var state: Int = -1

    val contentBox = dom.document.body .querySelector("#content")

    def changeView(): Unit = state match{
      case 0 => {
        val ui = new ProjectTemplate(scalatags.JsDom)
        contentBox.innerHTML = ""
        contentBox.appendChild(ui.projectsViewTemplate().render)
        updateProjects()
      }
      case 1 => contentBox.innerHTML = "Godverdomme zeg"
    }

    def changeStateTo(newState: Int) = {
      state = newState
      changeView()
    }

    changeStateTo(0)

    def updateProjects(): Unit =
      Ajax.get("/service/project").onComplete {
        case Success(xhr) =>
          val projectsM = decode[Seq[Project]](xhr.responseText)

          projectsM match {
            case Left(err) => dom.window.alert(err.toString)
            case Right(projects) =>
              val projectTarget = dom.document.body.querySelector("#projects")
              projectTarget.innerHTML = ""
              val ui = new ProjectTemplate(scalatags.JsDom)
              projectTarget.appendChild(ui.projectsTemplate(projects).render)
          }
        case Failure(err) => dom.window.alert(err.toString)
      }

    //def updateProject() = {}
    val goToProjectEl = dom.document.body.
      querySelector("#uwmoeke").
      asInstanceOf[HTMLButtonElement]

    goToProjectEl.onclick = (ev: Event) => {
      contentBox.innerHTML = "fuck you"
    }

    val addProjectEl = dom.document.body
      .querySelector("#addProject")
      .asInstanceOf[HTMLButtonElement]

    addProjectEl.onclick = (ev: Event) => {
      val projectNameEl = dom.document.body
        .querySelector("#projectName")
        .asInstanceOf[HTMLInputElement]
      val projectName = projectNameEl.value

      Ajax
        .post("/service/project", Project(projectName).asJson.noSpaces)
        .onComplete {
          case Success(xhr) =>
            projectNameEl.value = ""
            // This is a very, VERY crappy way of updating, don't do this in
            // your  project, come up with something that doesn't redraw the entire tree!
            updateProjects()
          case Failure(err) => dom.window.alert(err.toString)
        }
    }
  }

}
