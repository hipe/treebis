# Treebis

## Usage Examples

### Using DirAsHash to write files
_quick and dirty way to make files and folders from a hash:_

(see: test-for-doc.rb - 'using dir as hash')

In the above example we see a way to write two folders with three files.

The class we make included DirAsHash and used its `hash_to_dir()` method.

For the hash elements that are strings, the keys become filenames and the string become file contents.  For the hash values that are themselves hashes, the hash key becomes a folder name, and so on.

The last line shows that the files were written successfully.

### Making and Running a Treebis Task
_minimal treebis example_

The above example was just for giving us some files to work with.  In practice it is not a very practical way to write files of any significant length, if for no other reason than for how unreadable it would be (and hence useless to be in ruby.)[^in_prac]

Let's say you want to write a script that installs these files to some arbitrary directory.  *That* is exactly what our little Treebis here is for.

Let's say also you don't want to write all the files to the target location, just those files matching a fileglob pattern.  Furthermore let's say you want to process one of the files as an erb template, for which we will provide values to substitute for placeholders in the document:

(see: test-for-doc.rb - 'first task')

Above we see that a Task object is composed of a code block composed of a sequence of commands, many of which are wrappers around the familiar FileUtils methods.

Treebis tasks usually work with files _from_ a location and apply them _on_ another location (either copying files directly or interpolating them along the way, or applying diff patches.)  A Task won't autmatically make any directories without explicitly being told to do so, so we make directories in two places above.

In the last line we see that the net result of this is that the task wrote to the target directory two of the three files from the source directory, and in one of the files it interpolated the erb template with values that we passed to it.

I really wanted to show you the pretty colors in the output but I must get going!

<br />
<hr />
### _Footnotes_ ###

[^in_prac]: In practice I *do* however write files this way for many of my tests, including the one that generated the examples for this file ;)
