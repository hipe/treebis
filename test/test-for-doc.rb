require 'minitest/spec'
require 'treebis'
require 'nandoc/spec-doc'
require 'nandoc/testlib/minitest-extlib'
require 'pp'

MiniTest::Unit.autorun

class String
  def unindent
    this = (/\A([ \t]*)/ =~ self && $1)
    this = /^#{Regexp.escape(this)}/m
    ret = gsub(this, '')
    ret
  end
end

describe 'TestForDoc' do
  NanDoc::SpecDoc.include_to(self)
  Treebis::PersistentDotfile.include_to(self, 'treebis.persistent.json')

  def prompt
    @prompt ||= NanDoc::MockPrompt.new(self)
  end

  it 'using dir as hash' do
    @mydir = empty_tmpdir('make-some-files')
    FileUtils.cd(@mydir) do
      nandoc.record_ruby
      require 'treebis'

      class FilemakerPro
        include Treebis::DirAsHash # hash_to_dir(), dir_as_hash()

        def make_files in_folder
          hash = {
            'some-file.txt' => "happy mother's day, <%= name %>",
            'lists' => {
              'blah.txt' => 'foo',
              'favorite-foods.txt' => <<-HERE.gsub(/^ */,'')
                two bite brownies
                peanut cluster things
              HERE
            }
          }
          hash_to_dir(hash, in_folder)
        end
      end

      FilemakerPro.new.make_files('./foo')
      nandoc.out(<<-HERE.unindent
         ["./foo/lists",
          "./foo/lists/blah.txt",
          "./foo/lists/favorite-foods.txt",
          "./foo/some-file.txt"]
        HERE
      ) do
        PP.pp Dir['./foo/**/*']
      end
      nandoc.record_ruby_stop
    end
  end

  it 'first task' do
    run_this_test_again 'using dir as hash'
    FileUtils.cd(@mydir) do
      nandoc.record_ruby
      task = Treebis::Task.new do
        from './foo'
        mkdir_p 'lists'
        copy 'lists/*foods.txt'
        erb 'some-file.txt'
      end

      FileUtils.mkdir('./output')
      erb_values = {:name => 'dear mom'}
      task.erb_values(erb_values).on('./output').run

      nandoc.out(<<-'HERE'.unindent
{"lists"=>{"favorite-foods.txt"=>"two bite brownies\npeanut cluster things\n"},
 "some-file.txt"=>"happy mother's day, dear mom"}
      HERE
      ) do
        PP.pp(Treebis::DirAsHash.dir_as_hash('./output'))
      end
      nandoc.record_ruby_stop
    end
  end
  
  it 'persistent dotfile' do
    mydir = empty_tmpdir('persitent-dotfile')
    FileUtils.cd(mydir) do
      nandoc.record_ruby
      require 'treebis'
      module SomeMod
        extend self # cute way to get a singleton
        Treebis::PersistentDotfile.include_to(self, 'somefile.json')
      end
      
      module OtherMod
        extend self        
        Treebis::PersistentDotfile.include_to(self, 'somefile.json')
      end
      
      nandoc.inspect SomeMod.persistent_get('foo'), 'nil'
      
      SomeMod.persistent_set('foo','bar')
      nandoc.inspect SomeMod.persistent_get('foo'), '"bar"'
      
      nandoc.inspect OtherMod.persistent_get('foo'), '"bar"'
      nandoc.record_ruby_stop
      nandoc.story 'see contents'
      nandoc.record_ruby
      nandoc.out(<<-HERE.unindent
        {
          "foo": "bar"
        }
      HERE
      ) do
        puts File.read('somefile.json')
      end
      nandoc.record_ruby_stop
    end    
  end

  # @todo etc
  def run_this_test_again name
    filter = /\Atest_\d{4}_#{name.gsub(/[^_a-z0-9]/i,'_')}\Z/
    mm = self.class.test_methods.grep(filter)
    case mm.size
    when 0; fail("failed to find methods named #{name.inspect} with #{re}")
    when 1;
    else fail("found too many methods for #{name}: #{mm.join(', ')}")
    end
    send(mm.first)
  end
end
