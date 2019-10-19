package sgit.localChangeRepo

import sgit.io.{ConsolePrinter, CreateFile, ReadFile, SearchingTools, WriteFile}
import sgit.objects.StagedLine

object CommitCommand {
  /**
   * Create a commit
   * @param message : commit message
   * @return the commit sha key
   */
  def commit(message: String): Option[String] = {
    if(SearchingTools.searchSgitFolder()) {
      //retrieve the last commit
      val lastCommit: Option[String] = SearchingTools.findLastCommit()
      //retrieve the file in the staged file
      val stagedFiles: Option[Seq[StagedLine]] = ReadFile.readStaged()
      if (stagedFiles.isEmpty) {
        ConsolePrinter.display("No files have been staged.")
        None
      }
      else {
        val filesToWrite: Seq[StagedLine] = stagedFiles.get
        if (lastCommit.isDefined) {
          //retrieve the content of the last commit
          val lastCommitContent: Seq[StagedLine] = ReadFile.readCommit(lastCommit.get)

          //create the content of the new commit, replace the old version of file in last commit with the new version
          val newContent: Seq[StagedLine] = createContentNewCommit(lastCommitContent, filesToWrite)

          //create the commit file with the newContent, the untracked file(s) in the last commit and the old ones
          val newCommit: String = CreateFile.createCommit((lastCommit.get, ""), newContent.concat(filesToWrite).distinct, message)
          // write the new commit on the current branch file
          CreateFile.writeHeadsFile(ReadFile.readHEAD(), newCommit)
          //clear the staged file
          WriteFile.clearStaged()
          ConsolePrinter.display("Succes, the commit is created.")
          Some(newCommit)
        } else {
          //create the commit file and object
          val newCommit: String = CreateFile.createCommit(("", ""), filesToWrite, message)
          // write the new commit on the current branch file
          CreateFile.writeHeadsFile(ReadFile.readHEAD(), newCommit)
          //clear the staged file
          WriteFile.clearStaged()
          ConsolePrinter.display("Succes, the commit is created.")
          Some(newCommit)
        }
      }
    } else {
      ConsolePrinter.display("Do an sgit init.")
      None
    }
  }

  /**
   * Create the content of the new commit: replace the old version of file(s) in last commit with the new ones contains in staged file.
   * @param lastCommitContent : last commit content
   * @param stagedFileContent:  staged file content
   * @return the content of the new commit
   */
  def createContentNewCommit (lastCommitContent: Seq[StagedLine], stagedFileContent: Seq[StagedLine] ): Seq[StagedLine]= {
    //list of file paths contains in the staged file
    val filesPath: Seq[String] = stagedFileContent.map(f => f.path)

    lastCommitContent.map(f => {
      if (filesPath.contains(f.path)) {
        stagedFileContent.filter(file => file.path == f.path).head
      } else f
    })
  }

}
