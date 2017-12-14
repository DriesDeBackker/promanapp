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
import scalatags.Text

object ProManApp {

  def main(args: Array[String]): Unit = {

    var frontEndProjects: Seq[Project] = Seq()
    //def getProject(name: String): Project = frontEndProjects.filter(_.hasName(name)).head

    var contentBox = dom.document.body .querySelector("#content")
    val projectUi = new ProjectTemplate(scalatags.JsDom)
    val entryUi = new EntryTemplate(scalatags.JsDom)

    def getHTMLElement(id: String) = dom.document.body.querySelector("#" + id)
    def getButtonElement(id: String) = getHTMLElement(id).asInstanceOf[HTMLButtonElement]
    def getInputElement(id: String) = getHTMLElement(id).asInstanceOf[HTMLInputElement]

    var doneHidden = false
    var todoHidden = false

    def goToEntries(projectName: String): Unit = {
      doneHidden = false
      todoHidden = false
      contentBox.innerHTML = ""
      contentBox.appendChild(entryUi.entriesViewTemplate(projectName).render)
      getButtonElement("backToProjects").onclick = (ev: Event) =>
        goToProjects()
      getButtonElement("addEntry").onclick = (ev: Event) =>
        addEntry(projectName)
      getHTMLElement("undoneEntriesTitle").asInstanceOf[HTMLDivElement].onclick = (ev: Event) =>
        toggleUnDone(projectName)
      getHTMLElement("doneEntriesTitle").asInstanceOf[HTMLDivElement].onclick = (ev: Event) =>
        toggleDone(projectName)
      getInputElement("entryName").onclick = (ev: Event) =>
        clearWarning()
      updateEntries(projectName)
      clearWarning()
    }

    def toggleDone(projectName: String): Unit = {
      if (doneHidden) {
        doneHidden = false
        getHTMLElement("doneEntries").setAttribute("style", "display:block;")
      } else {
        doneHidden = true
        getHTMLElement("doneEntries").setAttribute("style", "display:none;")
      }
    }

    def toggleUnDone(projectName: String): Unit = {
      if (todoHidden) {
        todoHidden = false;
        getHTMLElement("undoneEntries").setAttribute("style", "display:block")
      } else {
        todoHidden = true;
        getHTMLElement("undoneEntries").setAttribute("style", "display:none;")
      }
    }

    def goToProjects(): Unit = {
      contentBox.innerHTML = ""
      contentBox.appendChild(projectUi.projectsViewTemplate().render)
      getButtonElement("addProject").onclick = (ev: Event) =>
        addProject()
      getInputElement("projectName").onclick = (ev: Event) =>
        clearWarning()
      updateProjects()
      clearWarning()
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
              val doneEntriesTarget = getHTMLElement("doneEntries")
              doneEntriesTarget.innerHTML=""
              val undoneEntriesTarget = getHTMLElement("undoneEntries")
              undoneEntriesTarget.innerHTML=""
              for (entry <- entries) {
                val entryDiv: Element = entryUi.entryDivTemplate(entry).render
                val entryInput: Element = entryUi.entryInputTemplate(entry).render
                val entryButton: Element = entryUi.entryButtonTemplate(entry).render
                entryInput.asInstanceOf[HTMLInputElement].onchange = (ev: Event) =>
                  changeEntryContent(projectName, entry.id, entryInput.asInstanceOf[HTMLInputElement].value)
                entryDiv.appendChild(entryInput)
                entryDiv.appendChild(entryButton)
                if (entry.done) {
                  doneEntriesTarget.appendChild(entryDiv)
                  entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) => {
                    markAsUnDone(projectName, entry.id)
                  }
                }
                else {
                  undoneEntriesTarget.appendChild(entryDiv)
                  entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) => {
                    markAsDone(projectName, entry.id)
                  }
                }
              }
          }
        case Failure(err) => dom.window.alert(err.toString)
      }

    def warn(warning: String) = getHTMLElement("warningBox").innerHTML = warning
    def clearWarning() = getHTMLElement("warningBox").innerHTML = ""

    def checkProjectName(projectName: String): (Boolean, String) = {
      if (projectName == "") (false, "No name specified.")
      else if (frontEndProjects.exists(_.name == projectName)) (false, "This name is already in use.")
      else (true, "")
    }

    def addProject(): Unit = {
      val projectNameEl = getInputElement("projectName")
      val projectName = projectNameEl.value
      val checkNameResponse = checkProjectName(projectName)
      val accepted = checkNameResponse._1
      val error = checkNameResponse._2
      if (accepted) {
        Ajax
          .post("/service/project", Project(projectName).asJson.noSpaces)
          .onComplete {
            case Success(xhr) =>
              projectNameEl.value = ""
              // This is a very, VERY crappy way of updating, don't do this in
              // your  project, come up with something that doesn't redraw the entire tree!
              updateProjects()
            case Failure(err) => warn(err.toString)
          }
      } else warn(error)
    }

    def checkEntityContent(entityContent: String): (Boolean, String) = {
      if (entityContent == "") (false, "Insert content.")
      else (true, "")
    }

    def addEntry(projectName: String): Unit = {
      val entryContentEl = getInputElement("entryName")
      val entryContent = entryContentEl.value
      val checkContentResponse = checkEntityContent(entryContent)
      val accepted = checkContentResponse._1
      val error = checkContentResponse._2
      if (accepted) {
        Ajax
          .post("/service/project/" + projectName + "/add/" + entryContent, "")
          .onComplete {
            case Success(xhr) =>
              entryContentEl.value = ""
              // This is a very, VERY crappy way of updating, don't do this in
              // your  project, come up with something that doesn't redraw the entire tree!
              updateEntries(projectName)
            case Failure(err) => warn(err.toString)
          }
      } else warn(error)
    }

    def markAsDone(projectName:String, entryId:Int): Unit = {
      Ajax
        .post("/service/project/" + projectName + "/" + entryId + "/markasdone","")
        .onComplete {
          case Success(xhr) =>
            updateEntries(projectName)
          case Failure(err) => dom.window.alert(err.toString)
        }
    }

    def markAsUnDone(projectName:String, entryId:Int): Unit = {
      Ajax
        .post("/service/project/" + projectName + "/" + entryId + "/markasundone", "")
        .onComplete {
          case Success(xhr) =>
            updateEntries(projectName)
          case Failure(err) => dom.window.alert(err.toString)
        }
    }

    def changeEntryContent(projectName:String, entryId:Int, entryContent: String): Unit = {
      Ajax
        .post("/service/project/" + projectName + "/" + entryId + "/changecontent/" + entryContent)
        .onComplete {
          case Success(xhr) =>
            updateEntries(projectName)
          case Failure(err) => dom.window.alert(err.toString)
        }
    }
  }

}
