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
    def getProject(name: String): Project = frontEndProjects.filter(_.hasName(name)).head

    var contentBox = dom.document.body .querySelector("#content")
    val projectUi = new ProjectTemplate(scalatags.JsDom)
    val boardUi = new BoardTemplate(scalatags.JsDom)
    val entryUi = new EntryTemplate(scalatags.JsDom)

    def getHTMLElement(id: String) = dom.document.body.querySelector("#" + id)
    def getButtonElement(id: String) = getHTMLElement(id).asInstanceOf[HTMLButtonElement]
    def getInputElement(id: String) = getHTMLElement(id).asInstanceOf[HTMLInputElement]

    /*var doneHidden = false
    var todoHidden = false*/

    def goToBoards(projectName: String): Unit = {
      /*doneHidden = false
      todoHidden = false*/
      contentBox.innerHTML = ""
      contentBox.appendChild(boardUi.boardsViewTemplate(projectName).render)
      getButtonElement("addBoard").onclick = (ev: Event) =>
        addBoard(projectName)
      getButtonElement("backToProjects").onclick = (ev: Event) =>
        goToProjects()
      /*getHTMLElement("undoneEntriesTitle").asInstanceOf[HTMLDivElement].onclick = (ev: Event) =>
        toggleUnDone(projectName)
      getHTMLElement("doneEntriesTitle").asInstanceOf[HTMLDivElement].onclick = (ev: Event) =>
        toggleDone(projectName)*/
      getInputElement("boardName").onclick = (ev: Event) =>
        clearWarning()
      updateBoards(projectName)
      clearWarning()
    }

    /*
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
    }*/

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
                val entryInput: Element = entryUi.entryInputTemplate(boardName,entry).render
                val entryButton: Element = entryUi.entryButtonTemplate(entry).render
                entryInput.asInstanceOf[HTMLInputElement].onchange = (ev: Event) =>
                  changeEntryContent(projectName, boardName, entry.id, entryInput.asInstanceOf[HTMLInputElement].value)
                entryDiv.appendChild(entryInput)
                entryDiv.appendChild(entryButton)
                if (entry.done) {
                  doneEntriesTarget.appendChild(entryDiv)
                  entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) => {
                    markAsUnDone(projectName,boardName, entry.id)
                  }
                }
                else {
                  todoEntriesTarget.appendChild(entryDiv)
                  entryButton.asInstanceOf[HTMLButtonElement].onclick = (ev:Event) => {
                    markAsDone(projectName, boardName, entry.id)
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

    def checkBoardName(projectName: String, boardName:String): (Boolean, String) = {
      if (boardName == "") (false, "No name specified")
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
          .post("/service/project/" + projectName + "/add", Board(boardName).asJson.noSpaces)
          .onComplete {
            case Success(xhr) =>
              boardNameEl.value = ""
              // This is a very, VERY crappy way of updating, don't do this in
              // your  project, come up with something that doesn't redraw the entire tree!
              updateBoards(projectName)
            case Failure(err) => warn(err.toString)
          }
      } else warn(error)
    }

    def checkEntityContent(entityContent: String): (Boolean, String) = {
      if (entityContent == "") (false, "Insert content.")
      else (true, "")
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
              entryContentEl.value = ""
              // This is a very, VERY crappy way of updating, don't do this in
              // your  project, come up with something that doesn't redraw the entire tree!
              updateEntries(projectName,boardName)
            case Failure(err) => warn(err.toString)
          }
      } else warn(error)
    }

    //TODO: make the methods below into one.
    def markAsDone(projectName:String, boardName: String, entryId:Int): Unit = {
      Ajax
        .post("/service/project/" + projectName + "/" + boardName + "/" + entryId + "/markasdone","")
        .onComplete {
          case Success(xhr) =>
            updateEntries(projectName, boardName)
          case Failure(err) => dom.window.alert(err.toString)
        }
    }

    def markAsUnDone(projectName:String, boardName:String, entryId:Int): Unit = {
      Ajax
        .post("/service/project/" + projectName + "/" + boardName + "/" + entryId + "/markasundone", "")
        .onComplete {
          case Success(xhr) =>
            updateEntries(projectName, boardName)
          case Failure(err) => dom.window.alert(err.toString)
        }
    }

    def changeEntryContent(projectName:String, boardName: String, entryId:Int, entryContent: String): Unit = {
      Ajax
        .post("/service/project/" + projectName + "/" + boardName + "/" + entryId + "/changecontent/" + entryContent)
        .onComplete {
          case Success(xhr) =>
            updateEntries(projectName, boardName)
          case Failure(err) => dom.window.alert(err.toString)
        }
    }

    def run(): Unit = {
      goToProjects()
    }

    run()
  }
}
