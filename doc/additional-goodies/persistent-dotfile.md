# treebis

## additional goodies

### PersistentDotfile

Initially an auxilliary class for testing, `PeristentDotfile` proved to be a useful little utility in its own right which I may end up getting more miles out of than Treebis itself in my other projects.  It simply:

  * wraps `Dir#tmpdir` which gives you a platform-independent api to (the platform dependent location of) a temporary directory.
  * gives you an awesome little persistent JSON structure that you can write to and read accross invocations to your app.[^sessions]

(see: test-for-doc.rb - 'persistent dotfile')


What's happening here is: 1) we're using the dubious pattern of making some modules and extending `self`, which is a quick and dirty way to get a singleton object[^sing].

2) We're enhancing the modules in question with `PersistentDotfile`. Instead of `include`ing it, we employ the `include_to` pseudo-pattern. It's like an `include()` but it makes explicit (some of) the creepy metaprogramming going on behind the scenes, and it allows us to pass parameters to the enhancement.

In this case, we are passing `'somefile.json'` which is an argument that tells the module where to write its persistent data.  (Making it a relative path like this, it will write the file to whatever the current working directory is when it goes to save it.)

As you can (sort of) see from the output, the first module writes the data to the file and the second module reads it.

Just for fun, let's see what it's writing to that file:

(see: test-for-doc.rb - 'persistent dotfile' -- 'see contents')


Simply amazing.

There're other mind-blowing things `PersistentDotfile` can achieve that will change your life, like making a temporary directory.

<br />
<br />
<hr />
### _Footnotes_ ###

[^sessions]:comparable to a really cheap & easy session façade you find in all web frameworks
[^sing]:the other kind of purist would have you use the [singleton module](http://www.ruby-doc.org/stdlib/libdoc/singleton/rdoc/files/singleton_rb.html) to make a singleton module.
