# Haskell Git (hgit)

## TL;DR 
A lightweight, pure Haskell library implementing core Git functionality for local-only repositories.

## High-level Overview

Git supports a number of commands, easily over a 100, of these, less than half are considered porcelain commands, high-level commands which most Git users are likely to be familiar with. The rest are known as plumbing commands and represent low-level operations which are most often used for scripting. For our project, we will implement a subset of the porcelain commands.

## Object Model & Workflow

User calls _git init_ which creates a _.hgit_ directory in the current directory. This directory will initially contain:
- A file titled _HEAD_, which will contain the relative file path e.g. "refs/heads/head", to the file that contains the object ID or OID for the commit object currently being pointed to by _HEAD_ e.g. the branch that the user is currently on.
- An objects directory, which will be two levels deep, the first level will contain directories named using the first two characters of the hexadecimal string (OID) formed from the SHA-1 hash of each file that's being tracked by hgit. The second level will contain files named using the latter 38 characters (An SHA-1 hash is 160 bits, and hexadecimal characters can each represent 4 bits since they're base-16, so 160/4 = 40 characters). The second level files will contain the serialized content of all objects: trees, commits, or blobs.
  - A commit object will contain the OID for it's root tree, the OID of the parent/s commit (no parent if root commit), and a timestamp of when the commit was made
  - A tree object will contain data on either blobs or other trees representing subdirectories. Both object types will have their associated OID and file or directory names plus their object type as blob or tree.
  - A blob object will simply contain serialized file content
- A refs directory, which will contain a _heads_ directory that will contain files, one for each local branch, named using that branch's name and its content will just be the OID for that branch, meaning the commit being pointed to by that branch

This directory will eventually also contain a file titled _index_ which will contain binary data on each file that is staged (it must have been staged via _git add_), with data on each file including the OID or SHA-1 hash and the path to the file from the root directory

__All of our implemented commands will rely on this object model, and they will not use any additional type of file or formatting to achieve our desired functionality.__ 

## Detailed Command Definitions incl. flags, args, etc.
  - hgit init: Will create the .hgit directory in the directory from where this command is given with an empty HEAD file, an empty objects directory, refs directory will just contain heads directory which will just contain an empty file called main (for main branch)
  - hgit add _______: Will update the index file with the added files, this will involve hashing the added file, seeing if it is already tracked and comparing the hashes, if same, then nothing more to do, if different, then update the hash and write to the objects directory with this file's contents (we're going to ignore optimizing here, we can later check to see if the existing hash is pointed to by any commit, if not we can delete it, maybe using a garbage collection thread which routinely checks for such blobs that are no longer connected to any commit or index)
    - Versions to support: 
      - hgit add filename: as expected (can support only single file name or recursively support many file names here)
      - hgit add -u: all tracked files in the entire working tree are updated (no pathspec allowed)
      - hgit add . : add all files in current directory and in subdirectories including hidden files and directories starting with a . (except for .hgit of course)
  - hgit commit: Create a new commit containing the current contents of the index and the given log message describing the changes. This means that a new commit object will be created and its parent will be the current commit object pointed to by HEAD, and its root tree will be created containing all the blobs or trees for the root dir of the project. This will involve DFS with and from the lowest level dir up we compare OIDs with what's in the objects directory, if it already exists, just point to it, otherwise create it. Keep in mind that this step will not be adding any new blob files so it will only be adding new tree objects (if any) and a commit object.
    - Versions to support:
      - hgit commit -m "commit msg" 
  - hgit branch: List, create, or delete branches
    - Versions to support:
      - hgit branch : list all branches with an asterisk to denote the current branch (Head pointer will contain this info)
      - hgit branch "branchname": creates a new branch that points to the commit pointed to by the file pointed to by Head ptr
      - hgit branch -d "branchname" : deletes the branch specified, and this would have to be a branch that the user is not currently on. Would involve just deleting the file corresponding to that branch in refs/heads
  - hgit switch: same functionality as hgit checkout via hgit switch "existing branchname"
  - hgit log: in reverse chronological order, give commit hash, current branch pointed to by Head, and then for each commit (assume only one parent for now since we will be implementing merge second block) list author name and email, timestamp of commit, and the commit message. When we do implement merge this will involve just picking the one parent, the branch into which the merge was made.
  - hgit status: Says what branch you're on and has 3 sections:
    - Changes to be committed: includes new files, modified files, deleted files
    - Changes not staged for commit: includes modified and deleted files (same file can be in changes to be committed and this section if you staged it and then made changes or deleted it)
    - Untracked files: files that haven't been staged, which includes new files, also ignored files if we ever implement .gitignore type functionality
