task :default => :test

me = "\e[35mtreebis\e[0m "

desc  "#{me}generate rcov coverage"
task :rcov do
  sh "rcov --exclude '.*gem.*' lib/treebis.rb"
end

desc "#{me}run them"
task :test do
  sh "ruby -w lib/treebis.rb"
  require File.dirname(__FILE__)+'/test/test-for-doc.rb'
end

desc "#{me}see if the tests for docs run"
task :doc do
  require File.dirname(__FILE__)+'/test/test_for_doc.rb'
end

desc "#{me}hack turns the installed gem into a symlink to this directory"
task :hack do
  kill_path = %x{gem which treebis}
  kill_path = File.dirname(File.dirname(kill_path))
  new_name  = File.dirname(kill_path)+'/ok-to-erase-'+File.basename(kill_path)
  FileUtils.mv(kill_path, new_name, :verbose => 1)
  this_path = File.dirname(__FILE__)
  FileUtils.ln_s(this_path, kill_path, :verbose => 1)
end


require 'jeweler'
# require 'nandoc'
# require 'nandoc/parse-readme'

Jeweler::Tasks.new do |s|
  s.authors = ['Chip Malice']
  # s.description = NanDoc::ParseReadme.description('README')
  s.description = <<-HERE.gsub(/^ +/,'')
    Treebis is a minimal, general scripting/task utility written in ruby 
    that wraps common actions for moving, copying and altering filetrees.
    It is geared towards things like generators.  It is comparable to a shell script
    that does a lot of mkdir, mv, cp commands etc.  
  HERE
  s.files =  FileList['[A-Z]*', '{bin,doc,generators,lib,test}/**/*']
  s.email = 'chip.malice@gmail.com'
  s.homepage = 'http://treebis.hipeland.org'
  s.name = 'treebis'
  s.rubyforge_project = 'treebis'
  # s.summary = NanDoc::ParseReadme.summary('README')
  s.summary = "minimal single-file rake-like task DSL for wrapping "<<
    "common filesystem tasks like copying files"
  s.add_dependency 'json', '~> 1.2.3'
end

