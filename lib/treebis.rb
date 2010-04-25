require 'fileutils'
require 'open3'

module Treebis
  module Config
    extend self
    @color = true
    def color?;    @color         end
    def no_color!; @color = false end
    def color!;    @color = true  end
  end
  @task_set = nil
  class << self
    def dir_task path
      taskfile = path + '/treebis-task.rb'
      require taskfile
      name = File.basename(path).to_sym
      task = @task_set[name]
      task.from path
      task
    end
    def tasks
      @task_set ||= TaskSet.new
    end
    def unindent content
      /\A(\s*)/ =~ content
      content.gsub(/^#{Regexp.escape($1)}/,'')
    end
  end
  module Sopen2
    @ui = nil
    module_function
    def sopen2 *a
      Open3.popen3(*a) do |ins,out,err|
        return [out.read, err.read]
      end
    end
    def sopen2assert *a
      out, err = sopen2(*a)
      if err.length > 0
        stderr.puts err
        fail(err.split("\n").first+"...")
      else
        out
      end
    end
    def stderr
      @stderr ||= ((@ui && @ui.respond_to?(:err)) ? @ui.err : $stderr)
    end
  end
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
    def initialize name=nil, &block
      @block = block
      @from = nil
      @name = name
      @ui = $stderr
      @ui_stack = []
    end
    attr_accessor :ui
    def from path
      @from = path
    end
    def on path
      RunContext.new(self, @block, @from, path, @ui)
    end
    def ui_capture &block
      ui_push
      instance_eval(&block)
      ui_pop
    end
    def ui_push
      @ui_stack.push @ui
      @ui = StringIO.new
    end
    def ui_pop
      it = @ui
      @ui = @ui_stack.pop
      resp = it.kind_of?(StringIO) ? (it.rewind && it.read) : it
      resp
    end
    class RunContext
      def initialize task, block, from_path, on_path, ui
        @task, @block, @from_path, @on_path = task, block, from_path, on_path
        @noop = false # no setters yet
        @overwrite = false
        @ui = ui
        @unindent = true # no setters yet
      end
      def apply diff_file
        pat, _ = normalize_from diff_file
        tgt, _ = normalize_on(/\A(.+)\.diff\Z/.match(diff_file)[1])
        out = Sopen2::sopen2assert('patch', '-u', tgt, pat)
        @ui.puts out
      end
      def copy path
        full, local = normalize_from path
        tgt = File.join(@on_path, local)
        skip = false
        if File.exist?(tgt) && File.read(full) == File.read(tgt)
          report_action(:identical, path)
          skip = true
        end
        FileUtils.cp(full, tgt, :verbose=>true, :noop=>@noop) unless skip
      end
      def move from, to
        fr_full, fr_local = normalize_on from
        to_full, fr_lcoal = normalize_on   to
        FileUtils.mv(fr_full, to_full, :verbose=>true, :noop=>@noop)
      end
      def from path
        @from_path = path
      end
      def mkdir_p_unless_exists dir_basename=nil
        if dir_basename
          full, short = normalize_on(dir_basename)
        else
          full, short = @on_path, @on_path
        end
        if File.exist?(full)
          report_action :exists, short
        else
          FileUtils.mkdir_p(full, :verbose => true, :noop=>@noop)
        end
      end
      alias_method :mkdir_p, :mkdir_p_unless_exists # temporary!!
      def rm_rf_if_exist
        path = @on_path
        if File.exist?(path) && rm_rf_sanity_check(path)
          FileUtils.remove_entry_secure(@on_path)
        end
      end
      alias_method :rm_rf, :rm_rf_if_exist # for now
      def run
        self.instance_eval(&@block)
      end
      def write path, content
        out_path, short = normalize_on path
        content = Treebis.unindent(content) if @unindent
        action = File.exist?(out_path) ? (
          File.read(out_path) == content ? :identical :
          ( @overwrite ? :changed : :"won't overwrite" )
        ) : :wrote
        xtra = nil
        if [:changed, :wrote].include?(action)
          b = fh = nil
          File.open(out_path,'w'){|fh| b = fh.write(content)}
          xtra = "(#{b} bytes)"
        end
        report_action action, short, xtra
      end
    private
      def normalize_from path
        fail("expecting leading dot: #{path}") unless short = undot(path)
        full = File.join(@from_path, short)
        [full, short]
      end
      def normalize_on path
        short = (thing = undot(path)) ? thing : path
        full = File.join(@on_path, short)
        [full, short]
      end
      def report_action reason, path, xtra=nil
        reason_s = if Config.color?
          stylize(reason.to_s, * StyleCodes[ReasonStyles[reason]])
        else
          reason.to_s
        end
        xtra = " #{xtra}" if xtra
        @ui.puts("      #{reason_s} #{path}#{xtra}") # look like nanoc
      end
      def rm_rf_sanity_check path
        # not sure what we want here
        fail('no') if @on_path.nil? || path != @on_path
        true
      end
      ReasonStyles = { :identical => :skip, :"won't overwrite" => :skip,
                       :changed   => :did,  :wrote => :did, :exists=>:skip }
      StyleCodes = {   :skip => [:bold, :red], :did  => [:bold, :green] }
      Codes = {:red=>'31', :green=>'32', :yellow=>'33',:bold=>'1',:blink=>'5'}
      def stylize str, *codenames
        return str if codenames == [nil]
        "\e["+codenames.map{|x| Codes[x]}.join(';')+"m#{str}\e[0m"
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

  # fakefs stuff removed in ac877

  module Treebis
    Persistent = './.treebis.json'
    @tmpdir = nil
    class << self
      def ridiculous_tree path
        dir = Dir.new(path)
        fail("huh?") unless '...' == (dir.read + dir.read)
        h = {}
        while entry = dir.read
          path2 = path + '/' + entry
          if File.directory?(path2)
            h[entry] = ridiculous_tree path2
          else
            h[entry] = File.read(path2)
          end
        end
        h
      end
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
        mkdir_p_unless_exists # makes output dir referred to in on() below
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

  class Treebis::MoreTestCase < Test::Unit::TestCase
    def test_rm_rf
      t = Treebis::Task.new do
        rm_rf
        mkdir_p
        write('foo.txt', "hi i'm foo")
      end
      out = Treebis.tmpdir+'/blearg'
      t.on(out).run
      t.on(out).run
      tree = Treebis.ridiculous_tree(out)
      assert_equal({"foo.txt"=>"hi i'm foo"}, tree)
    end
    def test_no_color
      t = Treebis::Task.new do
        report_action(:fake, "blah")
      end
      Treebis::Config.no_color!
      out = t.ui_capture{ on('x').run }
      Treebis::Config.color!
      assert_equal("      fake blah\n", out)
    end
    def test_no_overwrite
      dir = Treebis.tmpdir + '/no-overwrite/'
      FileUtils.remove_entry_secure(dir) if File.exist?(dir)
      t = Treebis::Task.new do
        mkdir_p_unless_exists
        write 'foo.txt','blah content'
      end
      t2 = Treebis::Task.new do
        write 'foo.txt','blah content again'
      end
      str1 = t.ui_capture{ on(dir).run }
      str2 = t2.ui_capture{ on(dir).run }
      assert_match( /wrote/, str1 )
      assert_match( /won/, str2 )
    end
    def setup_sourcedir
      src_dir = Treebis.tmpdir+'/banana'
      # return if File.exist?(src_dir)
      task = Treebis::Task.new do
        rm_rf
        mkdir_p
        mkdir_p 'dir1'
        write 'dir1/stooges.txt.diff', <<-FILE
          --- a/stooges.txt    2010-04-25 03:23:18.000000000 -0400
          +++ b/stooges.txt    2010-04-25 03:23:12.000000000 -0400
          @@ -1,2 +1,3 @@
           larry
          +moe
           curly
        FILE

        write 'stooges.orig.txt', <<-FILE
          larry
          curly
        FILE

        write 'treebis-task.rb', <<-FILE
          Treebis.tasks.task(:banana) do
            copy './stooges.orig.txt'
            mkdir_p_unless_exists './dir1'
            move './stooges.orig.txt', './dir1/stooges.txt'
            apply './dir1/stooges.txt.diff'
          end
        FILE
      end
      task.on(src_dir).run
    end
    def test_move
      setup_sourcedir
      task = Treebis.dir_task(Treebis.tmpdir+'/banana')
      out_dir = Treebis.tmpdir+'/test-move-output'
      FileUtils.remove_entry_secure(out_dir) if File.exist?(out_dir)
      FileUtils.mkdir_p(out_dir)
      task.on(out_dir).run
      tree = Treebis.ridiculous_tree(out_dir)
      assert_equal({"dir1"=>{"stooges.txt"=>"larry\nmoe\ncurly\n"}}, tree)
    end
  end
  Test::Unit::UI::Console::TestRunner.run(Treebis::MoreTestCase)

  class Treebis::Blah < Test::Unit::TestCase
    def test_sopen2
      e = assert_raises(RuntimeError) do
        Treebis::Sopen2.sopen2assert('patch', '-u', 'foo', 'bar')
      end
      assert_match(/Can't open patch file bar/, e.message)
    end
  end
  Test::Unit::UI::Console::TestRunner.run(Treebis::Blah)
end
