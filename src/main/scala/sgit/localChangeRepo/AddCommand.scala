package sgit.localChangeRepo

import better.files.File
import sgit.io.{ConsolePrinter, CreateFile, ReadFile, RepoSearching, SearchingTools, WriteFile}
import sgit.objects.{Blob, Commit, StagedLine}

import scala.util.matching.Regex

object AddCommand {

  /**
   * Check the argument type of add command and add the files to staged state.
   * @param files : sequence of string which can be a dot, a regex or filenames
   */
  def addAccordingTypeArg(files :Seq[String]): Unit = {
    if(SearchingTools.searchSgitFolder()) {
      if (files.nonEmpty) {
        if (files.head == ".") {
          //Search file in the user repo
          val retrieveFiles: Seq[File] = RepoSearching.searchAllDirectoryFile(".sgit/").filterNot(f => f.isDirectory)
          //Remove the files that are not modify
          val filesToAdd = SearchingTools.findUnmodifyFiles(retrieveFiles, ReadFile.readCommitProperties(SearchingTools.findLastCommit()))
          //Create files in objects directory
          val blobs: Seq[Blob] = CreateFile.createObjectBlob(filesToAdd)
          //write the staged file
          WriteFile.writeStaged(StagedUpdating.updateStagedFile(blobs))
          ConsolePrinter.display("Success, the files have been added.")
        }
        else {
          val regexFilename: Regex = "^[a-zA-Z0-9_/]+\\.[A-Za-z]*$".r
          if (regexFilename.matches(files.head)) {
            //Search file in the user repo
            val retrievesFiles: Seq[File] = RepoSearching.searchDirectoryFile(files)
            //Remove the files that are not modify
            val filesToAdd = SearchingTools.findUnmodifyFiles(retrievesFiles, ReadFile.readCommitProperties(SearchingTools.findLastCommit()))
            //Create files in objects directory
            val blobs: Seq[Blob] = CreateFile.createObjectBlob(filesToAdd)
            //write the staged file
            WriteFile.writeStaged(StagedUpdating.updateStagedFile(blobs))
            ConsolePrinter.display("Success, the files have been added.")
          } else {
            //Search file in the user repo
            val retrieveFiles: Seq[File] = RepoSearching.searchDirectoryFile(files.head.r)
            //Remove the files that are not modify
            val filesToAdd = SearchingTools.findUnmodifyFiles(retrieveFiles, ReadFile.readCommitProperties(SearchingTools.findLastCommit()))
            //Create files in objects directory
            val blobs: Seq[Blob] = CreateFile.createObjectBlob(filesToAdd.filterNot(f => f.isDirectory))
            //write the staged file
            WriteFile.writeStaged(StagedUpdating.updateStagedFile(blobs))
            //retrieveFiles.filter(f => f.isDirectory)
            ConsolePrinter.display("Success, the files have been added.")
          }
        }
      } else ConsolePrinter.display("Enter an argument after the add command.")
    } else ConsolePrinter.display("Do a sgit init.")
  }

}
