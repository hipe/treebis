## treebis

### summmary
Treebis is a minimal, general scripting/task utility that wraps common actions for moving, copying and altering filetrees.  It is geared towards things like generators.  It is comparable to a shell script that does a lot of mkdir, mv, cp commmands etc.

### what it is:
  - task wrapper for external commands that make filetrees, e.g.
    - maybe the generators in things like rails, ramaze, nandoc
  - copies filetrees to filetrees
  - removes filetrees from filetrees
  - applies diffs to filetrees
  - different ways to represent trees and diffs - heredocs, diffs, filesystem.
  - under 500 lines of code (?) (~300 SLOC ATTOTW)
  - near 100% test coverage (?)

### what it is not:
  - a vcs or vcs wrapper (version control system)
  - atomic
  - although it is tempting it does not wrap the output of FileUtils methods.
      mkdir_p, cp, mv, rm are passed verbose flags and write verbose messages
      to $stdout.  This may be added in the future for more attractive output.
      Also with a few exceptions no error checking is done.  This merely
      passes these requests on to FileUtils.

### Frequently Asked Questions

  #### Q: why use it?
  #### A: Because you want a consistent way to wrap these common tasks that doesn't explicitly rely on shelling out to the underlying system, or other hodgepodges.  (also see 'why did you make this?' below)

  #### Q: why not use it?
  #### A: Because it doesn't do what you want or it does what you do not want.

  #### Q: why is it named "Treebis?"
  #### A: because it rhymes with "Jeebus."

  #### Q: why did you make this?
  #### A: by the third or fourth time i found myself re-writing this same kind of thing for different projects, or bleeding from its absence, i decided to abstract it.  It's more readable than a bunch of FileUtils statements,  it paves the way for possible future enhancements like atomicitiy and units of work; and wouldn't it be nice if every generator of every project used the same library?

### requirements
  - ruby 1.8.7
  - GNU patch 2.5.8 (if the diff-patching functionality is to be used)
      (most versions of patch will likely work; it uses unified diffs.)

### installation
  @todo

### usage
  @todo

### future promises made today:
  - dry run
  - erb
  - two-pass units of work !!??
