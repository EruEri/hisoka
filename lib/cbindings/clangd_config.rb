require 'open3'


def failwith(message)
    puts message
    exit 1
end

command = "pkg-config --cflags chafa ncursesw"

stdout, status = Open3.capture2(command)

if status != 0
    failwith "Pkg fail"
end

args = stdout.chars.map do |c|
    case c
    when ' '
        ', '
    when "\n"
        ' '
    else
        c
    end
end.join


puts "CompileFlags:"
puts "  Add: [#{args}]"
puts "  Compiler: clang"