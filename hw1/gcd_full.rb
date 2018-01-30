#!/usr/bin/ruby

def gcdR(m, n)
  n == 0 ? m : gcdR(n, m % n)
end

def gcdI(m, n)
  while n != 0
    t = n
    n = m % n
    m = t
  end
  m
end

if ARGV.length != 2
	puts "We need two numbers for the script"
	puts "ruby gcd_full.rb [NUMBER] [NUMBER]"
	exit
end

puts gcdR(ARGV[0].to_i, ARGV[1].to_i)
puts gcdI(ARGV[0].to_i, ARGV[1].to_i)

