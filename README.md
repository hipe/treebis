## treebis

### summmary
Treebis is a minimal, general scripting/task utility that wraps common actions for moving, copying and altering filetrees.  It is geared towards things like generators.

### what it is:
  - task wrapper for external commands that make filetrees, e.g.
    - maybe the generators in things like rails, ramaze, nandoc
  - copies filetrees to filetrees
  - removes filetrees from filetrees
  - applies diffs to filetrees
  - different ways to represent trees and diffs - heredocs, diffs, filesystem.
  - under 500 lines of code (?)
  - 100% test coverage (?)

### what it is not:
  - a vcs or vcs wrapper (version control system)

### Frequently Asked Questions

  #### Q: why use it?
  #### A: Because you want a consistent way to wrap these common tasks that doesn't explicitly rely on shelling out to the underlying system

  #### Q: why not use it?
  #### A: Because it doesn't do what you want or it does what you do not want.

  #### Q: why is it named "Treebis?"
  #### A: because it rhymes with "Jeebus."

  #### Q: why did you make this?
  #### A: by the third or fourth time i found myself re-writing this same kind of thing for different projects, or bleeding from its absence, i decided to abstract it.  And wouldn't it be nice if every generator of every project used the same library?

### requirements
  - ruby 1.8.7
  - GNU patch 2.5.8 (if patching functionality is to be used)

### installation
  @todo

### usage
  @todo

### future promises today
- dry run
- erb
