require 'minitest/spec'
require 'nandoc' # requires treebis so .. yeah

MiniTest::Unit.autorun

describe 'TestForDoc' do
  NanDoc::SpecDoc.include_to(self)
  Treebis::PersistentDotfile.include_to(self, 'treebis.persistent.json')

  def prompt
    @prompt ||= NanDoc::MockPrompt.new(self)
  end

  it 'make files with dir as hash' do
    mydir = empty_tmpdir('make-some-files')
    FileUtils.cd(mydir) do
      nandoc.record_ruby
      require 'treebis'

      class FilemakerPro
        include Treebis::DirAsHash # hash_to_dir(), dir_as_hash()

        def make_files in_folder
          hash = {
            'some-file.txt' => "happy mother's day",
            'lists' => {
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
      nandoc.inspect Dir['./foo/**/*'], <<-HERE.strip
    ["./foo/lists", "./foo/lists/favorite-foods.txt", "./foo/some-file.txt"]
      HERE
      nandoc.record_ruby_stop
    end
  end
end
