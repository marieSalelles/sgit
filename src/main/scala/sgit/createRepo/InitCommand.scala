package sgit.createRepo

import sgit.io.createRepository
import sgit.io.writeFile

object InitCommand {

  /**
   * Create and initialise the .sgit repository into the current folder.
   */
  def createTreeView() = {
    val status = createRepository.initialisation()
    if (status) {
      writeFile.writeHead("master")
      println("The repository is created.")
    }
    else println("The repository was already created.")

  }
}
