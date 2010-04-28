require 'benchmark'
include Benchmark

n = 1_000_000
str = "abc"
a = b = c = d = nil
Benchmark.benchmark(" "*7 + CAPTION, 7, FMTSTR, ">total:", ">avg:") do |x|
  ta = x.report("one:")   { for i in 1..n ; a = str[0..0]  == 'a'   end }
  tb = x.report("two:")   { for i in 1..n ; b = str[0].chr == 'a'   end }
  tc = x.report("tre:")   { for i in 1..n ; c = (/^a/ =~ str )      end }
  td = x.report("foe:")   { for i in 1..n ; d = (0==str.index('a')) end }
  [ta+tb+tc+td, (ta+tb+tc+td)/3]
end

puts "abc: #{a}#{b}#{c}#{d}"
