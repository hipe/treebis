require 'fileutils'

module Treebis
  class TaskSet
    def initialize
      @tasks = {}
    end
    def task name, &block
      fail("can't reopen task: #{name.inspect}") if @tasks.key?(name)
      @tasks[name] = Task.new(name,&block)
    end
    def [](name)
      @tasks[name]
    end
  end
  class Task
    def initialize name, &block
      @name = name
      @block = block
    end
    def on path
      Runner.new(self, @block, path)
    end
    class Runner
      def initialize task, block, on_path
        @task, @block, @on_path = task, block, on_path
        @ui = $stderr
      end
      def copy path
        full, local = normalize(path)
        tgt = File.join(@on_path, local)
        skip = false
        if File.exist?(tgt) && File.read(full) == File.read(tgt)
          @ui.puts("skipping #{path} (identical)")
          skip = true
        end
        FileUtils.cp(full, tgt, :verbose => true) unless skip
        self
      end
      def from path
        @from_path = path
        self
      end
      def mkdir_unless_exists
        unless File.exist?(@on_path)
          FileUtils.mkdir_p(@on_path, :verbose => true)
        end
      end
      def run
        self.instance_eval(&@block)
      end
    private
      def normalize path
        fail("expecting leading dot: #{path}") unless nodot = undot(path)
        full = File.join(@from_path, nodot)
        [full, nodot]
      end
      def undot path
        $1 if /^(?:\.\/)?(.+)$/ =~ path
      end
    end
  end
end

if [__FILE__, '/usr/bin/rcov'].include?($PROGRAM_NAME) # ick
  require 'test/unit'
  require 'test/unit/ui/console/testrunner'
  require 'tempfile'
  require 'json'

  # require 'fakefs/safe'
  # reasons we aren't using FakeFS:
  # 1) doesn't work like real FS e.g. with
  #   FileUtils.cp("foo.txt","bar/baz.txt") if "bar/" doesn't exist
  # 2) messes with tty streams, rendering ruby-debug useless
  # 3) doesn't take opts parameter for FileUtils.cp, so no verbose
  #
  # module FakeFS
  #   # really guys?
  #   module FileUtils
  #     alias_method :cp_without_options, :cp
  #     def cp(src, dest, opts={})
  #       if opts[:verbose]
  #         $stderr.puts("cp #{src} #{dest}")
  #       end
  #       cp_without_options src, dest
  #     end
  #   end
  # end
  #
  module Treebis
    Persistent = './.treebis.json'
    class << self
      @tmpdir = nil
      def tmpdir
        if @tmpdir.nil?
          @tmpdir = get_tmpdir
        elsif ! File.exist?(@tmpdir)
          @tmpdir = get_tmpdir
        end
        @tmpdir
      end
    private
      def get_tmpdir
        write = tmpdir = nil
        if File.exists? Persistent
          struct = JSON.parse(File.read(Persistent))
          if File.exist?(struct['tmpdir'])
            tmpdir = struct['tmpdir']
          else
            write = true
          end
        else
          struct = {}
          write = true
        end
        unless tmpdir
          require 'tempfile'
          tmpdir = Dir::tmpdir + '/treebis'
          FileUtils::mkdir_p(tmpdir,:verbose=>true)
        end
        if write
          struct['tmpdir'] = tmpdir
          File.open(Persistent,'w+') do |fh|
            fh.write JSON.pretty_generate(struct)
          end
        end
        tmpdir
      end
    end
  end
  class Treebis::TestCase < Test::Unit::TestCase
    def test_cant_reopen_tasks
      tasks = Treebis::TaskSet.new
      tasks.task(:same){}
      e = assert_raises(RuntimeError) do
        tasks.task(:same){}
      end
      exp = "can't reopen task: :same"
      assert_equal(exp, e.message)
    end
    def test_can_open_same_name_in_different_task_sets
      Treebis::TaskSet.new.task(:same){}
      Treebis::TaskSet.new.task(:same){}
    end
    def test_copy_one_file_nothing_exist
      out_dir = Treebis.tmpdir+'/out-dir'
      src_file = Treebis.tmpdir+'/baz.txt'
      per_file = Treebis::Persistent
      FileUtils.remove_entry_secure(out_dir) if File.exist?(out_dir)
      FileUtils.remove_entry_secure(src_file) if File.exist?(out_dir)
      FileUtils.remove_entry_secure(per_file) if File.exist?(per_file)
      test_copy_one_file
    end
    def test_copy_one_file_almost_nothing_exist
      out_dir = Treebis.tmpdir+'/out-dir'
      src_file = Treebis.tmpdir+'/baz.txt'
      per_file = Treebis::Persistent
      FileUtils.remove_entry_secure(out_dir) if File.exist?(out_dir)
      FileUtils.remove_entry_secure(src_file) if File.exist?(out_dir)
      if File.exist?(per_file)
        struct = JSON.parse(File.read(per_file))
        if File.exist?(struct["tmpdir"])
          FileUtils.remove_entry_secure(struct["tmpdir"])
        end
      end
      test_copy_one_file
    end
    def test_copy_one_file_a_bunch_of_tmpdir_crap
      out_dir = Treebis.tmpdir+'/out-dir'
      src_file = Treebis.tmpdir+'/baz.txt'
      FileUtils.remove_entry_secure(out_dir) if File.exist?(out_dir)
      FileUtils.remove_entry_secure(src_file) if File.exist?(out_dir)
      Treebis.instance_variable_set('@tmpdir',nil)
      Treebis.tmpdir # gets to a hard to reach line
      test_copy_one_file
    end
    def test_copy_one_file
      # doc start
      content = "i am the content of\na file called baz.txt\n"
      File.open(Treebis.tmpdir+'/baz.txt','w+') do |fh|
        fh.puts(content)
      end
      tasks = Treebis::TaskSet.new
      tasks.task(:default) do
        mkdir_unless_exists # makes output dir referred to in on() below
        from Treebis.tmpdir # the source directory
        copy('./baz.txt')
      end
      tasks[:default].on(Treebis.tmpdir+'/out-dir/').run()
      output = File.read(Treebis.tmpdir+'/out-dir/baz.txt')
      # doc stop
      assert_equal(content, output)
    end
  end
  Test::Unit::UI::Console::TestRunner.run(Treebis::TestCase)
end
