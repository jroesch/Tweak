#!/usr/bin/ruby
require 'net/http'

ARGV.each do |x|
  puts x
  if x == "-s"
    `cd Projects/Tweak; sbt run`
  end
end

while true 
  begin
    print "tweak> "
    code = gets
    uri = URI('http://localhost:8080/')
    res = Net::HTTP.post_form(uri, :code => code)
    puts "=> " + res.body
  rescue StandardError
    puts "Error: issue connecting to the REPLServer, retry the REPL with -s the option."
  end
end


