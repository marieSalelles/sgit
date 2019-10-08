package sgit.createRepo

import sgit.io.CreateRepository
import sgit.io.WriteFile

object InitCommand {

  /**
   * Create and initialise the .sgit repository into the current folder.
   */
  def createTreeView() :Unit = {
    val status = CreateRepository.initialisation()
    if (status) {
      WriteFile.writeHead("master")
      println("The repository is created.")
    }
    else println("An error as occurred. Please try again.")
  }
}
