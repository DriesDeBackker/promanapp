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

    var view = "init"

    var frontEndProjects: Seq[Project] = Seq()
    def getProject(name: String): Project = frontEndProjects.filter(_.hasName(name)).head

    var contentBox = dom.document.body .querySelector("#content")
    val projectUi = new ProjectTemplate(scalatags.JsDom)
    val boardUi = new BoardTemplate(scalatags.JsDom)
    val entryUi = new EntryTemplate(scalatags.JsDom)

    def getHTMLElement(id: String) = dom.document.body.querySelector("#" + id)
    def getButtonElement(id: String) = getHTMLElement(id).asInstanceOf[HTMLButtonElement]
    def getInputElement(id: String) = getHTMLElement(id).asInstanceOf[HTMLInputElement]

    def goToBoards(projectName: String): Unit = {
      view = projectName
      contentBox.innerHTML = ""
      contentBox.appendChild(boardUi.boardsViewTemplate(projectName).render)
      getButtonElement("addBoard").onclick = (ev: Event) =>
        addBoard(projectName)
      getButtonElement("backToProjects").onclick = (ev: Event) =>
        goToProjects()
      getInputElement("boardName").onclick = (ev: Event) =>
        clearWarning()
      updateBoards(projectName)
      clearWarning()
    }

    def goToProjects(): Unit = {
      view = "projects"
      contentBox.innerHTML = ""
      contentBox.appendChild(projectUi.projectsViewTemplate().render)
      getButtonElement("addProject").onclick = (ev: Event) =>
        addProject()
      getInputElement("projectName").onclick = (ev: Event) =>
        clearWarning()
      updateProjects()
      clearWarning()
    }

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
                    goToBoards(project.name)
                projectTarget.appendChild(div)
              }
          }
        case Failure(err) => dom.window.alert(err.toString)
      }

    def updateBoards(projectName: String): Unit = {
      Ajax.get("service/project/" + projectName).onComplete {
        case Success(xhr) =>
          val boardsM = decode[Seq[Board]](xhr.responseText)
          boardsM match {
            case Left(err) => warn(err.toString)
            case Right(boards) =>
              getProject(projectName).boards = boards
              val boardsTarget = getHTMLElement("boards")
              boardsTarget.innerHTML = ""
              for (board <- boards) {
                val boardElement: Element = entryUi.boardTemplate(board.name).render
                boardsTarget.appendChild(boardElement)
                getHTMLElement(board.name + "TodoEntriesTitle").asInstanceOf[HTMLElement].onclick = (ev: Event) =>
                  toggleVisibility(getHTMLElement(board.name+"TodoEntries"))
                getHTMLElement(board.name + "DoneEntriesTitle").asInstanceOf[HTMLElement].onclick = (ev: Event) =>
                  toggleVisibility(getHTMLElement(board.name+"DoneEntries"))
                getButtonElement(board.name + "AddEntry").onclick = (ev: Event) =>
                  addEntry(projectName, board.name)
                updateEntries(projectName, board.name)
              }
          }
        case Failure(err) => warn(err.toString)
      }
    }

    def updateEntries(projectName: String, boardName: String): Unit =
      Ajax.get("/service/project/" + projectName + "/" + boardName).onComplete {
        case Success(xhr) =>
          val entriesM = decode[Seq[Entry]](xhr.responseText)
          entriesM match {
            case Left(err) => warn(err.toString)
            case Right(entries) =>
              getProject(projectName).getBoard(boardName).entries = entries
              val doneEntriesTarget = getHTMLElement(boardName + "DoneEntries")
              doneEntriesTarget.innerHTML=""
              val todoEntriesTarget = getHTMLElement(boardName + "TodoEntries")
              todoEntriesTarget.innerHTML=""
              for (entry <- entries) {
                val entryDiv: Element = entryUi.entryDivTemplate(boardName,entry).render
                val entryDate: Element = entryUi.entryDateTemplate(boardName,entry).render
                val entryInput: Element = entryUi.entryInputTemplate(boardName,entry).render
                val entryButton: Element = entryUi.entryButtonTemplate(entry).render
                entryInput.asInstanceOf[HTMLInputElement].onchange = (ev: Event) => {
                  updateEntry(projectName, boardName, Entry(entry.id, entry.date,entry.done, entryInput.asInstanceOf[HTMLInputElement].value))
                }
                entryDiv.appendChild(entryDate)
                entryDiv.appendChild(entryInput)
                entryDiv.appendChild(entryButton)
                if (entry.done) {
                  doneEntriesTarget.appendChild(entryDiv)
                  entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) => {
                    updateEntry(projectName,boardName,Entry(entry.id, entry.date, done=false, entry.content))
                  }
                }
                else {
                  todoEntriesTarget.appendChild(entryDiv)
                  entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) => {
                    updateEntry(projectName,boardName,Entry(entry.id, entry.date, done=true, entry.content))
                  }
                }
              }
          }
        case Failure(err) => warn(err.toString)
      }

    def warn(warning: String) = getHTMLElement("warningBox").innerHTML = warning
    def clearWarning() = getHTMLElement("warningBox").innerHTML = ""

    def checkProjectName(projectName: String): (Boolean, String) = {
      if (projectName == "") (false, "No name specified.")
      else if (projectName.contains(" ")) (false, "No spaces allowed")
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
              updateProjects()
            case Failure(err) => warn(err.toString)
          }
      } else warn(error)
    }

    def checkBoardName(projectName: String, boardName:String): (Boolean, String) = {
      if (boardName == "") (false, "No name specified")
      else if (boardName.contains(" ")) (false, "No spaces allowed")
      else if (getProject(projectName).boards.exists(_.name == boardName)) (false, "This name is already in use.")
      else (true, "")
    }

    def addBoard(projectName: String): Unit = {
      val boardNameEl = getInputElement("boardName")
      val boardName = boardNameEl.value
      val checkNameResponse = checkBoardName(projectName, boardName)
      val accepted = checkNameResponse._1
      val error = checkNameResponse._2
      if (accepted) {
        Ajax
          .post("/service/project/" + projectName + "/add",
            Board(boardName).asJson.noSpaces)
          .onComplete {
            case Success(xhr) =>
              boardNameEl.value = ""
              val boardsTarget = getHTMLElement("boards")
              val boardElement: Element = entryUi.boardTemplate(boardName).render
              boardsTarget.appendChild(boardElement)
              getHTMLElement(boardName + "TodoEntriesTitle").asInstanceOf[HTMLElement].onclick = (ev: Event) =>
                toggleVisibility(getHTMLElement(boardName+"TodoEntries"))
              getHTMLElement(boardName + "DoneEntriesTitle").asInstanceOf[HTMLElement].onclick = (ev: Event) =>
                toggleVisibility(getHTMLElement(boardName+"DoneEntries"))
              getButtonElement(boardName + "AddEntry").onclick = (ev: Event) =>
                addEntry(projectName, boardName)
              updateEntries(projectName, boardName)
            case Failure(err) => warn(err.toString)
          }
      } else warn(error)
    }

    def checkEntityContent(entityContent: String): (Boolean, String) = {
      if (entityContent == "") (false, "Insert content.")
      else (true, "")
    }

    def toggleVisibility(element: Element): Unit = {
      var style: String = element.getAttribute("style")
      if (style.contains("visibility: hidden")) style = style.replace("hidden", "visible")
      else if (style.contains("visibility: visible")) style = style.replace("visible", "hidden")
      element.setAttribute("style", style)
    }

    def addEntry(projectName: String, boardName: String): Unit = {
      val entryContentEl = getInputElement(boardName + "entryName")
      val entryContent = entryContentEl.value
      val checkContentResponse = checkEntityContent(entryContent)
      val accepted = checkContentResponse._1
      val error = checkContentResponse._2
      if (accepted) {
        Ajax
          .post("/service/project/" + projectName + "/" + boardName + "/add/" + entryContent, "")
          .onComplete {
            case Success(xhr) =>
              updateEntries(projectName, boardName)
            case Failure(err) => warn(err.toString)
          }
      } else warn(error)
    }

    def updateEntry(projectName: String, boardName: String, entry: Entry): Unit = {
      Ajax
        .post("/service/project/" + projectName + "/" + boardName + "/updateentry", entry.asJson.noSpaces)
        .onComplete {
          case Success(xhr) =>
            updateEntries(projectName, boardName)
          case Failure(err) => dom.window.alert(err.toString)
        }
    }

    goToProjects()
    import scala.scalajs.js.timers._
    def run(){
      setTimeout(10000) {
        if (view == "projects") {
          updateProjects()
        } else {
          updateBoards(view)
        }
        run()
      }
    }
    run()
  }
}
