package sgit.localChangeRepo

import better.files.File
import sgit.io.{ConsolePrinter, ReadFile, RepoSearching, SearchingTools}
import sgit.objects.StagedLine

object StatusCommand {

  /**
   * Classify the files according to their current stages and sub-stages.
   */
  def statusFile():Unit = {
    if(SearchingTools.searchSgitFolder()) {
      //working directory files
      val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile(".sgit/").filterNot(f => f.isDirectory)
      //staged files
      val stagedFile: Option[Seq[StagedLine]] = ReadFile.readStaged()

      //retrieve last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()

      //write the current branch
      ConsolePrinter.display("The current branch is " +ReadFile.readHEAD().split("/").toList.last)

      //if the last commit exists
      if (lastCommit.isDefined) {
        //print the last commit sha key
        ConsolePrinter.display("The last commit is " + lastCommit.get)
        //content of last commit
        val commitContent = ReadFile.readCommit(lastCommit.get)

        //modified files
        val modifiedFiles: Option[Seq[StagedLine]] = SearchingTools.searchedModifiedFiles(workingDirectory, commitContent)

        if (stagedFile.isDefined && modifiedFiles.isDefined) {
          //retrieve the modified files which are not in the staged file (not added)
          val modFilesWD: Seq[StagedLine] = modifiedFiles.get.diff(stagedFile.get)

          //files which are not staged (not added) but modified
          ConsolePrinter.printStatus("\u001B[31m"+"Not staged for commit:"+"\u001B[0m", "\u001B[35m"+"Modified "+"\u001B[0m", modFilesWD.map(f => f.path))

          // add the untracked files and deleted files (they are not in the staged file)
          untrackedFiles(workingDirectory, stagedFile)
          deleteFiles(workingDirectory)

          //modified files added to the staged file
          toBeCommitted(stagedFile.get, commitContent)

          //there are no files into the staged file (no files added) and there are modified files
        } else if (stagedFile.isEmpty && modifiedFiles.nonEmpty) {
          //files which are not staged (not added)
          ConsolePrinter.printStatus("\u001B[31m"+"Not staged for commit:"+"\u001B[0m", "\u001B[35m"+"Modified "+"\u001B[0m", modifiedFiles.get.map(f => f.path))

          // add the untracked files and deleted files (they are not in the staged file)
          untrackedFiles(workingDirectory, stagedFile)
          deleteFiles(workingDirectory)

          //there are no modified files but there are files into staged file (untracked files are added)
        } else if (modifiedFiles.isEmpty && stagedFile.nonEmpty) {
          untrackedFiles(workingDirectory, stagedFile)
          deleteFiles(workingDirectory)
          //staged files (added)
          ConsolePrinter.printStatus("\u001B[32m" +"Changes to be committed:"+"\u001B[0m", "\u001B[35m"+"Added "+"\u001B[0m", stagedFile.get.map(f => f.path))

          //there are no modification and no added files
        } else if (modifiedFiles.isEmpty && stagedFile.isEmpty) {
          untrackedFiles(workingDirectory, stagedFile)
          deleteFiles(workingDirectory)
        }
      } else {
        //there is no commit created
        //retrieve the staged files (added)
        if (stagedFile.nonEmpty) {
          untrackedFiles(workingDirectory, stagedFile)
          ConsolePrinter.printStatus("\u001B[32m"+"Changes to be committed:"+"\u001B[0m", "\u001B[35m"+"Added "+"\u001B[0m", stagedFile.get.map(f => f.path))
        }
      }
    } else ConsolePrinter.display("Do a sgit init before a sgit status.")
  }

  /**
   * Search and display the untracked files.
   * @param workingDirectory : working directory files
   * @param stagedFile : files into the staged file
   */
  def untrackedFiles(workingDirectory: Seq[File], stagedFile: Option[Seq[StagedLine]]): Unit= {

      //untracked files
      val untrackedFiles: Option[Seq[String]] = SearchingTools.searchedUntrackedFiles(workingDirectory,stagedFile)

      if(untrackedFiles.isDefined) ConsolePrinter.printStatus("\u001B[31m"+"Not staged for commit:"+"\u001B[0m","\u001B[35m"+"Untracked files "+"\u001B[0m",untrackedFiles.get)

  }

  /**
   * Search and display the deleted file.
   * @param workingDirectory : working directory file
   */
  def deleteFiles(workingDirectory: Seq[File]): Unit = {

    //retrieve last commit
    val lastCommit: Option[String] = SearchingTools.findLastCommit()

    if (lastCommit.isDefined) {
      //content of last commit
      val commitContent = ReadFile.readCommit(lastCommit.get)

      //deleted files
      val deletedFile: Option[Seq[String]] = SearchingTools.searchDeletedFiles(workingDirectory, commitContent)
      if (deletedFile.nonEmpty) ConsolePrinter.printStatus("\u001B[31m"+"Not staged for commit:"+"\u001B[0m","\u001B[35m"+"Deleted "+"\u001B[0m",deletedFile.get)
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
      ConsolePrinter.printStatus("\u001B[32m"+"Changes to be committed:"+"\u001B[0m","\u001B[35m"+"Added "+"\u001B[0m",addedFileIntoSgit.get.map(af => af.path))

      // retrieve the modified files already into sgit
      val fileAlreadyIntoSgit: Seq[StagedLine] = stagedFiles.diff(addedFileIntoSgit.get)

      if(fileAlreadyIntoSgit.nonEmpty) ConsolePrinter.printStatus("\u001B[32m"+"Changes to be committed:"+"\u001B[0m","\u001B[35m"+"Modified "+"\u001B[0m",fileAlreadyIntoSgit.map(mf => mf.path))
    }  else ConsolePrinter.printStatus("\u001B[32m"+"Changes to be committed:"+"\u001B[0m","\u001B[35m"+"Modified "+"\u001B[0m",stagedFiles.map(sf => sf.path))
  }

}
