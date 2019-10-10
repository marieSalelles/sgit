package sgit.createRepo

import sgit.io.{ConsolePrinter, CreateRepository, WriteFile}

object InitCommand {

  /**
   * Create and initialise the .sgit repository into the current folder.
   */
  def createTreeView() :Unit = {
    val status = CreateRepository.initialisation()
    if (status) {
      WriteFile.writeHead("master")
      ConsolePrinter.display("The repository is created.")
    }
    else ConsolePrinter.display("An error as occurred. Please try again.")
  }
}
