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
require 'nandoc'
require 'nandoc/parse-readme'

Jeweler::Tasks.new do |s|
  s.add_dependency 'json'
  s.authors = ['Chip Malice']
  s.description = NanDoc::ParseReadme.description('README')
  s.files =  FileList['[A-Z]*', '{bin,doc,generators,lib,test}/**/*']
  s.email = 'chip.malice@gmail.com'
  s.homepage = 'http://treebis.hipeland.org'
  s.name = 'treebis'
  s.rubyforge_project = 'treebis'
  s.summary = NanDoc::ParseReadme.summary('README')
end

