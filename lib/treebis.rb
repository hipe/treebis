require 'fileutils'
require 'json'
require 'open3'
require 'pathname'
require 'shellwords'
require 'tempfile'

module Treebis

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

  module Antecedent
    def init_path_antecedent
      @antecedent = {}
    end
    def path_antecedent domain, string
      if @antecedent[domain] && head =common_head(@antecedent[domain], string)
        @antecedent[domain] = [@antecedent[domain], string].max_by(&:length)
        path_antecedent_truncate head, string
      else
        @antecedent[domain] = string
        string
      end
    end
  private
    def path_antecedent_truncate common_head, string
      tail = string[common_head.length..-1]
      common_head =~ %r<(.{0,3}/?[^/]+/?)\Z>
      head = $1
      "...#{head}#{tail}"
    end
    def common_head str1, str2
      for i in 0..( [str1.length, str2.length].min - 1 )
        break unless str1[i] == str2[i]
      end
      ret = str1[0..i-1]
      ret
    end
  end
  # like Open3 but stay in the ruby runtime
  module Capture3
    # @return [result, out_string, err_string]
    def capture3 &block
      prev_out, prev_err, result = $stdout, $stderr, nil
      out_str, err_str = nil, nil
      $stderr = StringIO.new
      $stdout = StdoutStringIoHack.new
      begin
        result = block.call
      ensure
        $stderr.rewind
        out_str = $stdout.to_str
        err_str = $stderr.read
        $stdout = prev_out
        $stderr = prev_err
      end
      [result, out_str, err_str]
    end
    class StdoutStringIoHack < File
      private(*ancestors[1..2].map(&:public_instance_methods).flatten)
      these = %w(puts write)
      public(*these)
      def initialize
        super('/dev/null','w+')
        @buffer = StringIO.new
      end
      these.each do |this|
        alias_method "old_#{this}", this # avoid silly warnings
        define_method(this){|*a| @buffer.send(this, *a) }
      end
      def to_str
        @buffer.rewind
        @buffer.read
      end
    end
  end
  module Colorize
    Codes = {:bright=>'1', :red=>'31', :green=>'32', :yellow=>'33',
      :blue=>'34',:bold=>'1',:blink=>'5'}
    def colorize str, *codenames
      return str if codenames == [nil] || codenames.empty?
      "\e["+codenames.map{|x| Codes[x]}.join(';')+"m#{str}\e[0m"
    end
    module_function :colorize
  end
  module Config
    extend self
    @color = true
    def color?;    @color         end
    def no_color!; @color = false end
    def color!;    @color = true  end

    @default_prefix = ' '*6 # sorta like nanoc
    attr_accessor :default_prefix
  end
  class Fail < RuntimeError; end
  class FileUtilsAsClass
    # make it so we can effectively subclass FileUtils and override methods
    # and call up to the originals
    include FileUtils
    public :cp, :mkdir_p, :mv, :rm, :remove_entry_secure
  end
  module FilesystemAsHash
    #
    # Return an entire filsystem node as a hash whose files are represented
    # as strings holding the contents of the file and whose folders
    # are other such hashes.  The hash element keys are the entry strings.
    # Careful! the only real use for this so far is in testing.
    #
    def dir_as_hash path
      Hash[ Dir.new(path).each.map.reject{|x| 0==x.index('.')}.map { |e|
        p = path + '/' + e
        ['.','..'].include?(e) ? nil :
        [e, File.directory?(p) ?  dir_as_hash(p) : File.read(p) ]
      } ]
    end
  end
  class FileUtilsProxy < FileUtilsAsClass
    # We like the idea of doing FileUtils.foo(:verbose=>true) and outputting
    # whatever the response it to screen, but sometimes we want to format its
    # response a little more when possible.
    #
    include Antecedent, Capture3, Colorize
    def initialize &block
      @prefix = ''
      @pretty = false
      init_path_antecedent
      yield(self) if block_given?
    end
    attr_writer :ui
    def cp *args
      opts = args.last.kind_of?(Hash) ? args.last : {}
      ret, out, err = nil, "", nil
      keep = {:pretty_name_target => opts.delete(:pretty_name_target) }
      if ! @pretty
        ret = super(*args)
      else
        ret, out, err = capture3{ super(*args) }
        opts.merge!(keep)
        pretty_puts_cp ret, out, err, *args
      end
      ret
    end
    def prefix= str
      @pretty = true
      @prefix = str
    end
    def pretty!
      @pretty = true
    end
    def rm *a
      if @pretty
        b = capture3{ super(*a) }
        pretty_puts_rm(*(b+a))
      else
        ret = super(*a)
      end
      ret
    end
    def not_pretty!
      @pretty = false
    end
    def mkdir_p *args
      ret, out, err = nil, "", nil
      if File.exist?(args.first)
        err = "treebis: mkdir_p: exists: #{args.first}"
      elsif @pretty
        ret, out, err = capture3{ super(*args) }
      else
        ret = super
      end
      pretty_puts_mkdir_p ret, out, err, *args
      ret
    end
  private
    def pretty_puts_cp ret, out, err, *args
      fail("unexpected out: #{out.inspect} or ret: #{ret.inspect}") unless
        ret.nil? && out == ''
      opts = args.last.kind_of?(Hash) ? args.last : {}
      p1, p2 = pretty_puts_cp_paths(args[0], args[1], opts)
      # Regexp.escape(args[0..1].map{|x|Shellwords.escape(x)}.join(' '))
      matched =
      /\Acp #{Regexp.escape(args[0])} #{Regexp.escape(args[1])}(.*)\Z/ =~ err
      if err.empty?
        err = "no response? cp #{p1} #{p2}"
      elsif matched
        err = "cp #{p1} #{p2}#{$1}"
      end
      err.sub!(/\A(?:no response\? )?cp\b/){colorize('cp',:bright,:green)}
      ui.puts("%s%s" % [@prefix, err])
    end
    # use a variety of advanced cutting edge technology to determine
    # a shortest way to express unambiguesque-ly paths
    def pretty_puts_cp_paths path1, path2, opts
      path2 = if opts[:pretty_name_target]
        opts[:pretty_name_target]
      else
        s1 = Pathname.new(path2).relative_path_from(Pathname.new(path1)).to_s
        s2 = path_antecedent(:cp_tgt, path2)
        s3 = path2
        shortest, idx = [s1, s2, s3].each_with_index.min_by{|v, i|v.length}
        shortest
      end
      path1 = path_antecedent(:cp_src, path1)
      [path1, path2]
    end
    def pretty_puts_mkdir_p ret, out, err, *args
      fail("expecing mkdir_p never to write to stdout") unless out == ""
      return unless args.last.kind_of?(Hash) && args.last[:verbose]
      if @pretty
        # wierd that sometimes err is empty sometimes not
        fake_err = "mkdir -p #{ret}"
        fake_err.sub!(/\A(mkdir -p)/){ colorize($1, :bright, :green)}
        ui.puts sprintf("#{@prefix}#{fake_err}")
      else
        ui.puts(err) unless err == "" # emulate it
      end
    end
    def pretty_puts_rm ret, out, err, *a
      err = err.sub(/\Arm\b/){colorize('rm', :bright, :green)}
      ui.puts sprintf("#{@prefix}#{err}")
    end
    def ui
      $stderr # one day we will almost certainly make a setter for this
      # but one thing we *don't* want to do is ever set @ui equal to $stderr
      # because then capture3 won't work in tests.
    end
  end
  module PathString
    def no_leading_dot_slash str
      str.sub(/\A\.\//,'')
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
      include Colorize, PathString
      def initialize task, block, from_path, on_path, ui
        @task, @block, @from_path, @on_path = task, block, from_path, on_path
        @noop = false # no setters yet
        @opts = {}
        @overwrite = false
        @prefix = Config.default_prefix
        @ui = ui
        @file_utils = FileUtilsProxy.new do |fu|
          fu.prefix = @prefix
          fu.ui     = ui      # write to wherever the context writes to
        end
        @unindent = true # no setters yet
      end
      attr_accessor :file_utils, :opts, :prefix
      def apply diff_file
        pat, _ = normalize_from diff_file
        tgt, _ = normalize_on(/\A(.+)\.diff\Z/.match(diff_file)[1])
        if ! File.exist?(pat) && @opts[:patch_hack]
          return patch_hack(diff_file)
        end
        cmd = ['patch', '-u', tgt, pat]
        out, err = Sopen2::sopen2(*cmd)
        if err.length > 0
          cmd_str = cmd.shelljoin
          msg = sprintf(
            "Failed to run patch command: %s\nstdout was: %sstderr was: %s",
            cmd_str, out, err)
          fail msg
        else
          pretty_puts_apply out
        end
      end
      def copy path
        full, local = normalize_from path
        tgt = File.join(@on_path, local)
        skip = false
        if File.exist?(tgt) && File.read(full) == File.read(tgt)
          report_action :identical, path
        else
          file_utils.cp(full, tgt, :verbose=>true, :noop=>@noop,
            :pretty_name_target => no_leading_dot_slash(path)
          )
        end
      end
      def move from, to
        fr_full, fr_local = normalize_on from
        to_full, fr_lcoal = normalize_on   to
        file_utils.mv(fr_full, to_full, :verbose=>true, :noop=>@noop)
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
          file_utils.mkdir_p(full, :verbose => true, :noop=>@noop)
        end
      end
      alias_method :mkdir_p, :mkdir_p_unless_exists # temporary!!
      def remove path
        full, _ = normalize_on(path)
        file_utils.rm(full, :verbose => true, :noop => @noop)
      end
      def rm_rf_if_exist
        path = @on_path
        if File.exist?(path) && rm_rf_sanity_check(path)
          file_utils.remove_entry_secure(@on_path)
        end
      end
      alias_method :rm_rf, :rm_rf_if_exist # for now
      def run opts={}
        @opts = @opts.merge(opts.reject{|k,v| v.nil?}) # not sure about this
        self.instance_eval(&@block)
      end
      def write path, content
        out_path, short = normalize_on path
        content = Treebis.unindent(content) if @unindent
        action = if File.exist? out_path
          File.read(out_path) == content ? :identical :
          ( @overwrite ? :changed : :"won't overwrite" )
        else
          :wrote
        end
        xtra = nil
        if [:changed, :wrote].include?(action)
          b = fh = nil
          File.open(out_path,'w'){|fh| b = fh.write(content)}
          xtra = "(#{b} bytes)"
        end
        report_action action, short, xtra
      end
    private
      def fail(*a); raise ::Treebis::Fail.new(*a) end
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
      def patch_hack diff_file
        /\A(.+)\.diff\Z/ =~ diff_file
        use = $1 or fail("diff_file parse fail: #{diff_file}")
        copy use
      end
      def pretty_puts_apply out
        if /^\Apatching file (.+)\Z/ =~ out
          report_action :patched, $1
        else
          @ui.puts("#{prefix}#{out}")
        end
      end
      def report_action reason, msg=nil, xtra=nil
        reason_s = stylize(reason.to_s, reason)
        puts = ["#{prefix}#{reason_s}", msg, xtra].compact.join(' ')
        @ui.puts puts
      end
      def rm_rf_sanity_check path
        # not sure what we want here
        fail('no') if @on_path.nil? || path != @on_path
        true
      end
      ReasonStyles = { :identical => :skip, :"won't overwrite" => :skip,
                       :changed   => :did,  :wrote => :did, :exists=>:skip,
                       :patched   => :changed2, :notice => :changed2 }
      StyleCodes = {   :skip => [:bold, :red], :did  => [:bold, :green] ,
                       :changed2 => [:bold, :yellow] }
      def stylize str, reason
        return str unless Config.color?
        codes = StyleCodes[ReasonStyles[reason]]
        colorize(reason.to_s, * StyleCodes[ReasonStyles[reason]])
      end
      def undot path
        $1 if /^(?:\.\/)?(.+)$/ =~ path
      end
    end
  end
end

# Experimental extension for tests running tests with a persistent tempdir
#
module Treebis
  module PersistentDotfile
    class << self
      def extend_to(tgt, dotfile_path, opts={})
        opts = {:file_utils=>FileUtils, :dotfile_path=>dotfile_path}.
          merge(opts)
        tgt.extend ClassMethods
        tgt.persistent_dotfile_init opts
      end
      def include_to(mod, *a)
        extend_to(mod, *a)
        mod.send(:include, InstanceMethods)
      end
    end
    module ClassMethods
      attr_accessor :dotfile_path, :file_utils

      def persistent_dotfile_init opts
        @dotfile_path ||= opts[:dotfile_path]
        @file_utils   ||= opts[:file_utils]
        @tmpdir       ||= nil
      end

      #
      # if it exists delete it. create it. file_utils must be defined
      # @return path to new empty directory
      #
      def empty_tmpdir path, futils_opts={}
        futils_opts = {:verbose=>true}.merge(futils_opts)
        full_path = File.join(tmpdir, path)
        if File.exist? full_path
          file_utils.remove_entry_secure full_path, futils_opts
        end
        file_utils.mkdir_p full_path, futils_opts
        full_path
      end

      # Get a path to a temporary directory, suitable to be used in tests.
      # The contents of this directory are undefined, but it is writable
      # and as the name implies temporary so a given test should feel free
      # to erase it and create a new empty one at this same path if desired.
      # (see callee for details.)
      #
      def tmpdir
        if @tmpdir.nil?
          @tmpdir = get_tmpdir
        elsif ! File.exist?(@tmpdir) # check every time!
          @tmpdir = get_tmpdir
        end
        @tmpdir
      end

    private

      # Return the same tmpdir used in the previous run, if a "./treebis.json"
      # file exists in the cwd and it refers to a temp folder that still
      # exists.  Else make a new temp folder and write its location to this
      # file.  Using the same tempfolder on successive runs is one way to
      # allow us to look at the files it generates between runs.
      #
      def get_tmpdir
        write = tmpdir = nil
        if File.exists? dotfile_path
          struct = JSON.parse(File.read(dotfile_path))
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
          tmpdir = Dir::tmpdir + '/treebis'
          file_utils.mkdir_p(tmpdir,:verbose=>true)
        end
        if write
          struct['tmpdir'] = tmpdir
          File.open(dotfile_path,'w+') do |fh|
            fh.write JSON.pretty_generate(struct)
          end
        end
        tmpdir
      end
    end

    module InstanceMethods
      these = %w(tmpdir empty_tmpdir)
      these.each do |this|
        define_method(this){ |*a| self.class.send(this, *a) }
      end
    end
  end
end



if [__FILE__, '/usr/bin/rcov'].include?($PROGRAM_NAME) # ick
  # fakefs stuff removed in ac877, see notes there for why
  require 'test/unit'
  require 'test/unit/ui/console/testrunner'

  module ::Treebis::Test

    module TestAntecedents
      def setup_antecedents
        src = empty_tmpdir('sourceis')
        task.new do
          mkdir_p "foo/bar/baz"
          write "foo/bar/baz/alpha.txt", 'x'
          write "foo/bar/beta.txt", 'x'
          write "foo/gamma.txt", 'x'
        end.on(src).run
        src
      end
      def test_antecedents
        src = setup_antecedents
        fu = nil
        tt = task.new do
          from src
          fu = file_utils
          mkdir_p "foo/bar/baz"
          copy "foo/bar/baz/alpha.txt"
          copy "foo/bar/beta.txt"
          copy "foo/gamma.txt"
        end
        tgt = empty_tmpdir('targetis')
        aa, bb, cc = capture3{ tt.on(tgt).run }
        assert_equal [nil, ''], [aa,bb]
        penu, last = cc.split("\n")[-2..-1]
        assert penu.index("...bar/beta.txt foo/bar/beta.txt")
        assert last.index("...eis/foo/gamma.txt foo/gamma.txt")
      end

      def test_antecedents_raw
        fu = file_utils
        src = setup_antecedents
        tgt = empty_tmpdir('targetis')
        these = %w(    foo/bar/baz/alpha.txt
                       foo/bar/beta.txt
                       foo/gamma.txt          )
        fu.pretty!
        _, out, err = capture3 do
          fu.mkdir_p File.join(tgt,'foo/bar/baz')
          these.each do |foo|
            from, to = File.join(src,foo), File.join(tgt,foo)
            fu.cp from, to
          end
        end
        assert_equal '', out
        penu, last = err.split("\n")[-2..-1]
        assert penu.index(' ...bar/beta.txt ...bar/beta.txt')
        assert last.index('...eis/foo/gamma.txt ...tis/foo/gamma.txt')
      end
    end

    module TestColorAndRmAndMoveAndPatch
      def test_rm_rf
        t = task.new do
          rm_rf
          mkdir_p
          write('foo.txt', "hi i'm foo")
        end
        out = tmpdir+'/blearg'
        t.on(out).run
        t.on(out).run
        tree = dir_as_hash(out)
        assert_equal({"foo.txt"=>"hi i'm foo"}, tree)
      end
      def test_no_color
        t = task.new do
          report_action :fake, "blah"
        end
        Treebis::Config.no_color!
        out = t.ui_capture{ on('x').run }
        Treebis::Config.color!
        assert_equal("      fake blah\n", out)
      end
      def test_no_overwrite
        dir = tmpdir + '/no-overwrite/'
        Fu.remove_entry_secure(dir) if File.exist?(dir)
        t = task.new do
          mkdir_p_unless_exists
          write 'foo.txt','blah content'
        end
        t2 = task.new do
          write 'foo.txt','blah content again'
        end
        str1 = t.ui_capture{ on(dir).run }
        str2 = t2.ui_capture{ on(dir).run }
        assert_match( /wrote/, str1 )
        assert_match( /won/, str2 )
      end
      def setup_sourcedir
        src_dir = tmpdir+'/banana'
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
        task = Treebis.dir_task(tmpdir+'/banana')
        out_dir = tmpdir+'/test-move-output'
        file_utils.remove_entry_secure(out_dir) if File.exist?(out_dir)
        file_utils.mkdir_p(out_dir)
        task.on(out_dir).run
        tree = dir_as_hash(out_dir)
        assert_equal({"dir1"=>{"stooges.txt"=>"larry\nmoe\ncurly\n"}}, tree)
      end
    end

    module TestFileUtilsProxy
      def test_cp_not_pretty
        src_dir = empty_tmpdir('foo')
        tgt_dir = empty_tmpdir('bar')

        task.new{
          mkdir_p
          write 'some-file.txt', <<-X
            i am some text
          X
        }.on(src_dir).run

        task.new {
          from(src_dir)
          report_action :notice, "the below is not supposed to be pretty"
          file_utils.not_pretty!
          copy 'some-file.txt'
          mkdir_p 'emptoz'
          file_utils.pretty!
          report_action :notice, "the above was not pretty"
        }.on(tgt_dir).run

        act = dir_as_hash tgt_dir
        exp = {
          "some-file.txt"=>"i am some text\n",
          "emptoz" => {}
        }
        assert_equal(exp, act)
      end
      def test_pretty_puts_cp_unexpected_output
        e = assert_raises(RuntimeError) do
          file_utils.send(:pretty_puts_cp, false, '', '')
        end
        assert_match(/unexpected out/, e.message)
      end
    end


    module TestPatch
      def test_patch_fails
        src = empty_tmpdir 'patch/evil'
        task.new do
          write "badly-formed-patch-file.diff", <<-P
            i am not a good patch
          P
        end.on(src).run

        tgt = empty_tmpdir 'patch/innocent'
        patch_task = task.new do
          from src
          apply 'badly-formed-patch-file.diff'
        end
        e = assert_raises(Treebis::Fail) do
          patch_task.on(tgt).run
        end
        assert_match(/Failed to run patch command/, e.message)
      end
      def test_patch_hack
        task.new {
          write "some-file.txt", <<-X
            i am the new content
          X
        }.on(src = empty_tmpdir('src')).run
        task.new {
          write "some-file.txt", <<-X
            never see me
          X
        }.on(tgt = empty_tmpdir('tgt')).run
        task.new {
          from src
          opts[:patch_hack] = true
          apply "some-file.txt.diff"
        }.on(tgt).run
        assert_equal(
          {"some-file.txt"=>"i am the new content\n"},
          dir_as_hash(tgt)
        )
      end
      def test_unexpected_string_from_patch
        res, out, err = capture3 do
          task.new do
            pretty_puts_apply("i am another string")
          end.on(empty_tmpdir("blah")).run
        end
        assert_equal [nil, ""], [res, out]
        assert err.index("i am another string")
      end
    end

    module TestPersistentDotfile
      def test_persistent_dotfile_emtpy_tempdir
        blah1 = empty_tmpdir('blah/blah')
        blah2 = empty_tmpdir('blah/blah')
        assert_equal(blah1, blah2)
        assert_equal({}, dir_as_hash(blah2))
      end
    end

    module TestRemove
      def test_remove_pretty
        task.new {
          write "foo.txt", "bar baz"
        }.on(tgt = empty_tmpdir("foo")).run

        assert_equal({"foo.txt"=>"bar baz"}, dir_as_hash(tgt))

        task.new {
          remove "foo.txt"
        }.on(tgt).run

        assert_equal({}, dir_as_hash(tgt))
      end

      def test_remove_not_pretty
        task.new {
          write "foo.txt", "bar baz"
        }.on(tgt = empty_tmpdir("foo")).run

        assert_equal({"foo.txt"=>"bar baz"}, dir_as_hash(tgt))

        aa, bb, cc = capture3{
          tt = task.new do
            file_utils.not_pretty!
            remove "foo.txt"
          end
          tt.on(tgt).run
        }
        assert_match(/\Arm /, cc)
        assert aa.kind_of?(Array)
        assert_equal 1, aa.size
        assert_equal "", bb
        assert_equal({}, dir_as_hash(tgt))
      end

    end

    module TestSopen
      def test_sopen2_fails
        e = assert_raises(RuntimeError) do
          Treebis::Sopen2.sopen2assert('patch', '-u', 'foo', 'bar')
        end
        assert_match(/Can't open patch file bar/, e.message)
      end
      def test_sopen2_succeeds
        out = Treebis::Sopen2.sopen2assert('echo', 'baz')
        assert_equal "baz\n", out
      end
    end

    module TestTempdirAndTasksAndCopy
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
        out_dir = tmpdir+'/out-dir'
        src_file = tmpdir+'/baz.txt'
        per_file = self.class.dotfile_path
        file_utils.remove_entry_secure(out_dir) if File.exist?(out_dir)
        file_utils.remove_entry_secure(src_file) if File.exist?(src_file)
        file_utils.remove_entry_secure(per_file) if File.exist?(per_file)
        test_copy_one_file
      end
      def test_copy_one_file_almost_nothing_exist
        out_dir = tmpdir+'/out-dir'
        src_file = tmpdir+'/baz.txt'
        per_file = self.class.dotfile_path
        file_utils.remove_entry_secure(out_dir) if File.exist?(out_dir)
        file_utils.remove_entry_secure(src_file) if File.exist?(src_file)
        if File.exist?(per_file)
          struct = JSON.parse(File.read(per_file))
          if File.exist?(struct["tmpdir"])
            file_utils.remove_entry_secure(struct["tmpdir"])
          end
        end
        test_copy_one_file
      end
      def test_copy_one_file_a_bunch_of_tmpdir_crap
        out_dir = tmpdir+'/out-dir'
        src_file = tmpdir+'/baz.txt'
        file_utils.remove_entry_secure(out_dir) if File.exist?(out_dir)
        file_utils.remove_entry_secure(src_file) if File.exist?(src_file)
        self.class.instance_variable_set('@tmpdir',nil)
        tmpdir # gets to a hard to reach line
        test_copy_one_file
      end
      def test_copy_one_file
        the_tmpdir = tmpdir
        # doc start
        content = "i am the content of\na file called baz.txt\n"
        File.open(the_tmpdir+'/baz.txt','w+') do |fh|
          fh.puts(content)
        end
        tasks = Treebis::TaskSet.new
        tasks.task(:default) do
          mkdir_p_unless_exists # makes output dir referred to in on() below
          from the_tmpdir # the source directory
          copy('./baz.txt')
        end
        tasks[:default].on(the_tmpdir+'/out-dir/').run()
        output = File.read(the_tmpdir+'/out-dir/baz.txt')
        # doc stop
        assert_equal(content, output)
      end
    end


    class TestCase < ::Test::Unit::TestCase
      include Treebis::FilesystemAsHash, Treebis::Capture3
      include TestAntecedents
      include TestColorAndRmAndMoveAndPatch
      include TestFileUtilsProxy
      include TestPatch
      include TestPersistentDotfile
      include TestRemove
      include TestTempdirAndTasksAndCopy
      include TestSopen

      futils_prefix = sprintf( "%s%s --> ", Treebis::Config.default_prefix,
        Treebis::Colorize.colorize('for test:', :bright, :blue) )

      file_utils = Treebis::FileUtilsProxy.new do |fu|
        fu.prefix = futils_prefix
      end

      Treebis::PersistentDotfile.include_to( self,
        './treebis.json', :file_utils => file_utils )

      @file_utils = file_utils
      define_method( :file_utils ){ file_utils }
      alias_method :fu, :file_utils
      class << self
        attr_accessor :file_utils
      end
      def task; Treebis::Task end
    end

    ::Test::Unit::UI::Console::TestRunner.run(TestCase)
  end
end
