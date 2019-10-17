package sgit.refsOperation

import sgit.io.{ConsolePrinter, CreateRepository, ReadFile, SearchingTools, WriteFile}
import sgit.objects.{Blob, StagedLine}

object CheckoutCommand {

  /**
   * Change the current working directory for the files save with the given argument
   * @param identifier: branch name or tag name or commit sha key
   */
  def checkout(identifier: String)= {
    if (SearchingTools.searchSgitFolder()) {
      //search if it is a branch name
      val isLastCommit: Option[String] = SearchingTools.findLastCommit()
      if (isLastCommit.isDefined){
        //check if the user has added files
        val isFileAdded: Option[List[StagedLine]] = ReadFile.readStaged()
        if (!isFileAdded.isDefined) {
          if (SearchingTools.searchBranch(identifier)) {
            val commitSha: String = ReadFile.readBranchCommit("refs/heads/" + identifier)
            val commitContent: Seq[StagedLine] = ReadFile.readCommitProperties(Some(commitSha)).get.files
            val commitBlobs: Seq[Blob] = ReadFile.readBlobFile(commitContent, Seq())
            //build the new working directory
            commitBlobs.foreach(b => CreateRepository.builtRepository(b))
            //write the current branch into the HEAD file
            WriteFile.writeHead(identifier)

            // search if it is a tag name
          } else if (SearchingTools.searchTag(identifier)) {
            val commitSha: String = ReadFile.readTagCommit("refs/tags/" + identifier)
            val commitContent: Seq[StagedLine] = ReadFile.readCommitProperties(Some(commitSha)).get.files
            //build the new working directory
            val commitBlobs: Seq[Blob] = ReadFile.readBlobFile(commitContent, Seq())
            commitBlobs.foreach(b => CreateRepository.builtRepository(b))

          } else if (SearchingTools.searchCommit(identifier)) {
            val commitContent: Seq[StagedLine] = ReadFile.readCommitProperties(Some(identifier)).get.files
            val commitBlobs: Seq[Blob] = ReadFile.readBlobFile(commitContent, Seq())
            //build the new working directory
            commitBlobs.foreach(b => CreateRepository.builtRepository(b))

          } else ConsolePrinter.display("The argument is not a branch or a tag or a commit.")
        } else ConsolePrinter.display("Some files are added but not committed. Please do a commit before a checkout.")
      } else ConsolePrinter.display("Do at least one commit.")
    }
  }
}
