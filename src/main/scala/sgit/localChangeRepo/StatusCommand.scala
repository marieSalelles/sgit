package sgit.localChangeRepo

import better.files.File
import sgit.io.{ConsolePrinter, ReadFile, RepoSearching, SearchingTools}
import sgit.objects.StagedLine

object StatusCommand {

  def statusFile() = {
    val workingDirectory: Seq[File] = RepoSearching.searchAllDirectoryFile()
    val stagedFile :Option[Seq[StagedLine]]= ReadFile.readStaged()
    if (stagedFile.isDefined){
      val untrackedFiles: Option[Seq[File]] = SearchingTools.searchedUntrackedFiles(workingDirectory)
      //val deletedFiles: Option[Seq[StagedLine]] = SearchingTools.searchDeletedFiles(workingDirectory, stagedFile)
    } else {
      ConsolePrinter.display("Untracked files :")
      workingDirectory.map(f => println(f.name))
    }
  }

}
