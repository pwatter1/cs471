#!/usr/bin/ruby

def ack(m, n)
  if m == 0
    n + 1
  elsif n == 0
    ack(m - 1, 1)
  else
    ack(m - 1, ack(m, n - 1))
  end
end

if ARGV.length != 2
	puts "We need two numbers for the script"
	puts "ruby gcd_full.rb [NUMBER] [NUMBER]"
	exit
end

puts ack(ARGV[0].to_i, ARGV[1].to_i)

