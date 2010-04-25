# development dependencies:
# fakefs

task :default => :test

desc "generate rcov coverage"
task :rcov do
  sh "rcov --exclude '.*gem.*' lib/treebis.rb"
end

desc "run them"
task :test do
  sh "ruby -w lib/treebis.rb"
end
