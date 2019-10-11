package sgit.localChangeRepo

import better.files.File
import sgit.io.{ConsolePrinter, ReadFile, RepoSearching, SearchingTools}
import sgit.objects.StagedLine

object StatusCommand {

  /**
   * Classify the files according to their current stages and sub-stages.
   */
  def statusFile():Unit = {
    //working directory files
    val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile(".sgit/").filterNot(f => f.isDirectory)
    //staged files
    val stagedFile :Option[Seq[StagedLine]]= ReadFile.readStaged()

    //retrieve last commit
    val lastCommit: Option[String] = SearchingTools.findLastCommit()

    //if the last commit exists
    if (lastCommit.isDefined) {
      //content of last commit
      val commitContent = ReadFile.readCommit(lastCommit.get)

      //modified files
      val modifiedFiles: Option[Seq[StagedLine]] = SearchingTools.searchedModifiedFiles(workingDirectory,commitContent)

      if (stagedFile.isDefined && modifiedFiles.isDefined) {
        //retrieve the modified files which are not in the staged file (not added)
        val modFilesWD: Seq[StagedLine] = modifiedFiles.get.diff(stagedFile.get)

        //files which are not staged (not added) but modified
        ConsolePrinter.display("Not staged for commit:")
        ConsolePrinter.display("Modified ")
        ConsolePrinter.displayList(modFilesWD.map(f => f.path))
        // add the untracked files and deleted files (they are not in the staged file)
        untrackedFiles(workingDirectory, stagedFile)
        deleteFiles(workingDirectory, stagedFile)

        //staged files (added)
        ConsolePrinter.display("Changes to be committed:")
        toBeCommitted(stagedFile.get, commitContent)


        //there are no files into the staged file (no files added) and there are modified files
      } else if(stagedFile.isEmpty && modifiedFiles.nonEmpty) {
        //files which are not staged (not added)
        ConsolePrinter.display("Not staged for commit:")
        ConsolePrinter.display("Modified ")
        ConsolePrinter.displayList(modifiedFiles.get.map(f => f.path))

        // add the untracked files and deleted files (they are not in the staged file)
        untrackedFiles(workingDirectory,stagedFile)
        deleteFiles(workingDirectory,stagedFile)

        //there are no modified files but there are files into staged file (untracked files are added)
      } else if (modifiedFiles.isEmpty && stagedFile.nonEmpty) {
        //staged files (added)
        ConsolePrinter.display("Changes to be committed:")
        ConsolePrinter.display("Added ")
        ConsolePrinter.displayList(stagedFile.get.map(f => f.path))

        //there are no modification and no files addd
      } else if (modifiedFiles.isEmpty && stagedFile.isEmpty ) {
        untrackedFiles(workingDirectory,stagedFile)
        deleteFiles(workingDirectory,stagedFile)
      }
    } else {
      //there is no commit created
      //retrieve the untracked files
      ConsolePrinter.display("Not staged for commit:")
      untrackedFiles(workingDirectory, stagedFile)
      //retrieve the staged files (added)
      if (stagedFile.nonEmpty) {
        ConsolePrinter.display("Changes to be committed:")
        ConsolePrinter.display("Added ")
        ConsolePrinter.displayList(stagedFile.get.map( f => f.path))
      }
    }
  }

  /**
   * Search and display the untracked files.
   * @param workingDirectory : working directory files
   * @param stagedFile : staged files
   */
  def untrackedFiles(workingDirectory: Seq[File], stagedFile: Option[Seq[StagedLine]]): Unit= {

      //untracked files
      val untrackedFiles: Option[Seq[String]] = SearchingTools.searchedUntrackedFiles(workingDirectory,stagedFile)

      if(untrackedFiles.isDefined) {
        ConsolePrinter.display("Untracked files ")
        ConsolePrinter.displayList(untrackedFiles.get)
      }
  }

  /**
   * Search and display the deleted file.
   * @param workingDirectory : working directory file
   * @param stagedFile :staged files
   */
  def deleteFiles(workingDirectory: Seq[File], stagedFile: Option[Seq[StagedLine]]): Unit = {

    //retrieve last commit
    val lastCommit: Option[String] = SearchingTools.findLastCommit()

    if (lastCommit.isDefined) {
      //content of last commit
      val commitContent = ReadFile.readCommit(lastCommit.get)

      //deleted files
      val deletedFile: Option[Seq[String]] = SearchingTools.searchDeletedFiles(workingDirectory, commitContent)
      if (deletedFile.nonEmpty) {
        ConsolePrinter.display("Deleted ")
        ConsolePrinter.displayList(deletedFile.get)
      }
    }
  }

  /**
   * Classify the files in the to be committed stage: Added or Modify sub stage.
   * @param stagedFiles : files into the staged file
   * @param commitContent : commit files
   */
  def toBeCommitted(stagedFiles: Seq[StagedLine], commitContent: Seq[StagedLine]) :Unit = {
    //retrieve the new files added by the user into sgit
    val addedFileIntoSgit: Option[Seq[StagedLine]] = SearchingTools.toBeCommittedFileAdded(stagedFiles, commitContent)

    if (addedFileIntoSgit.nonEmpty) {
      ConsolePrinter.display("Added ")
      ConsolePrinter.displayList(addedFileIntoSgit.get.map(af => af.path))

      // retrieve the modified files already into sgit
      val fileAlreadyIntoSgit: Seq[StagedLine] = stagedFiles.diff(addedFileIntoSgit.get)

      if(fileAlreadyIntoSgit.nonEmpty) {
        ConsolePrinter.display("Modified ")
        ConsolePrinter.displayList(fileAlreadyIntoSgit.map(mf => mf.path))
      }
    }
    else {
      ConsolePrinter.display("Modified ")
      ConsolePrinter.displayList(stagedFiles.map(sf => sf.path))
    }
  }

}
