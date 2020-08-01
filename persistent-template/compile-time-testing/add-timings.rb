#!ruby -w

directory = ARGV[0]
puts "Looking for data in #{directory}"

Dir.chdir(directory)
filenames = Dir.glob('*.dump-timings')

totals = []

filenames.each do |name|
  text = File.read(name)
  lines = text.split("\n")
  total = 0
  lines.each do |line|
    start, time = line.split("time=")
    total += time.to_f
  end

  totals << total
end

mean = totals.inject(0.0) { |sum, el| sum + el } / totals.size
puts "Mean is #{mean}ms"
