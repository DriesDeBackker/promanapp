package be.kuleuven.proman

import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.ext.Ajax.InputData
import org.scalajs.dom.raw._

import scalatags.Text.all._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object ProManApp {

  def main(args: Array[String]): Unit = {

    var frontEndProjects: Seq[Project] = Seq()
    def getProject(name: String): Project = frontEndProjects.filter(_.hasName(name)).head

    var contentBox = dom.document.body .querySelector("#content")
    val projectUi = new ProjectTemplate(scalatags.JsDom)
    val entryUi = new EntryTemplate(scalatags.JsDom)

    def getHTMLElement(id: String) = dom.document.body.querySelector("#" + id)
    def getButtonElement(id: String) = getHTMLElement(id).asInstanceOf[HTMLButtonElement]
    def getInputElement(id: String) = getHTMLElement(id).asInstanceOf[HTMLInputElement]

    def goToEntries(projectName: String): Unit = {
      contentBox.innerHTML = ""
      contentBox.appendChild(entryUi.entriesViewTemplate().render)
      getButtonElement("backToProjects").onclick = (ev: Event) =>
        goToProjects()
      getButtonElement("addEntry").onclick = (ev: Event) =>
        addEntry(projectName)
      updateEntries(projectName)
    }

    def goToProjects(): Unit = {
      contentBox.innerHTML = ""
      contentBox.appendChild(projectUi.projectsViewTemplate().render)
      getButtonElement("addProject").onclick = (ev: Event) =>
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
              frontEndProjects = projects
              val projectTarget = getHTMLElement("projects")
              projectTarget.innerHTML = ""
              val ui = new ProjectTemplate(scalatags.JsDom)
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
              val project:Project = frontEndProjects.filter(_.hasName(projectName)).head
              project.entries = entries
              val entryTarget = getHTMLElement("entries")
              entryTarget.innerHTML = ""
              for (entry <- entries) {
                val entryDiv: Element = entryUi.entryTemplate(entry).render
                val entryButton: Element = entryUi.entryButtonTemplate(entry).render
                entryDiv.appendChild(entryButton)
                if (entry.done) entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) => {
                  entry.setToUndone()
                  updateEntry(projectName, entry.name)
                }
                else entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) => {
                  entry.setToDone()
                  updateEntry(projectName, entry.name)
                }
                entryTarget.appendChild(entryDiv)
              }
          }
        case Failure(err) => dom.window.alert(err.toString)
      }

    def addProject(): Unit = {
      val projectNameEl = getInputElement("projectName")
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

    def addEntry(projectName: String): Unit = {
      val entryNameEl = getInputElement("entryName")
      val entryName = entryNameEl.value
      Ajax
        .post("/service/project/" + projectName + "/add",
          Entry(entryName).asJson.noSpaces)
        .onComplete {
          case Success(xhr) =>
            entryNameEl.value = ""
            // This is a very, VERY crappy way of updating, don't do this in
            // your  project, come up with something that doesn't redraw the entire tree!
            updateEntries(projectName)
          case Failure(err) => dom.window.alert(err.toString)
        }
    }

    def updateEntry(projectName:String, entryName:String): Unit = {
      Ajax
        .post("/service/project/" + projectName + "/update",
          getProject(projectName).getEntry(entryName).asJson.noSpaces)
        .onComplete {
          case Success(xhr) =>
            updateEntries(projectName)
          case Failure(err) => dom.window.alert(err.toString)
        }
    }
  }

}
