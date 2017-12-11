package be.kuleuven.proman

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.raw._

import scalatags.Text.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object ProManApp {

  def main(args: Array[String]): Unit = {

    var contentBox = dom.document.body .querySelector("#content")
    val projectUi = new ProjectTemplate(scalatags.JsDom)
    val entryUi = new EntryTemplate(scalatags.JsDom)

    def goToEntries(projectName: String): Unit = {
      contentBox.innerHTML = ""
      contentBox.appendChild(entryUi.entriesViewTemplate().render)
      dom.document.body.querySelector("#backToProjects")
        .asInstanceOf[HTMLButtonElement].onclick = (ev: Event) =>
          goToProjects()
      updateEntries(projectName)
    }

    def goToProjects(): Unit = {
        contentBox.innerHTML = ""
        contentBox.appendChild(projectUi.projectsViewTemplate().render)
        dom.document.body.querySelector("#addProject")
        .asInstanceOf[HTMLButtonElement].onclick = (ev: Event) =>
          addProject()
        updateProjects()
    }

    goToProjects()

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
              //TODO: add event listeners with for loop below
              for (project <- projects) {
                val div: Element = ui.projectTemplate(project).render
                  div.asInstanceOf[HTMLDivElement].onclick = (ev: Event) =>
                    goToEntries(project.name)
                projectTarget.appendChild(div)
              }
          }
        case Failure(err) => dom.window.alert(err.toString)
      }

    def updateEntries(projectName: String): Unit =
      Ajax.get("/service/project/" + projectName).onComplete {
        case Success(xhr) =>
          val entriesM = decode[Seq[Entry]](xhr.responseText)
          entriesM match {
            case Left(err) => dom.window.alert(err.toString)
            case Right(entries) =>
              val entryTarget = dom.document.body.querySelector("#entries")
              entryTarget.innerHTML = ""
              for (entry <- entries) {
                val entryDiv: Element = entryUi.entryTemplate(entry).render
                val entryButton: Element = entryUi.entryButtonTemplate(entry).render
                entryDiv.appendChild(entryButton)
                if (entry.done) entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) =>
                  dom.document.querySelector("#entriesMessageBox").innerHTML = "marking as undone"
                else entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) =>
                  dom.document.querySelector("#entriesMessageBox").innerHTML = "marking as done"
                entryTarget.appendChild(entryDiv)
              }
          }
        case Failure(err) => dom.window.alert(err.toString)
      }

    /*val projectdivs: NodeList = dom.document.body.getElementsByClassName("projectdiv")
    for (i: Int <- 0 until projectdivs.length) {
      projectdivs.item(i).asInstanceOf[HTMLDivElement].onclick = (ev: Event) => {
        fuckyou()
      }
    }*/

    def addProject(): Unit = {
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
