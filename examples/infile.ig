#!/usr/bin/env igel

# set i 0
# while [lt i 10] {
#     print "\"i\" is" i
#     set i [add i 1]
# }
# # print a b c

print "Defining without args..."
proc greet-world {} {
    print "Hello, world!"
    # greet "world" # At the moment, this works, because no check is performed at run-time.
}

print "Defining with args..."
proc greet {target: string ""} {
    write "Hello, " target
    print "!"
}
print "Defined!"

greet-world
greet-world
greet-world

greet "Earth"

# print "Assigning to built-in"
# set gt 1
# set set 1

print "Testing a variable"

write "Two statements..." ; print "...on the same line."

var n: int 0
print n

set n 1
print n

set n 2
print n

set n [add n 1] 
print n

print "Testing constants:"

write "Expect 1 ... -> "
const c: int 1
print c
# set c 1

write "Expect \"Thing\" (without quotes) ... -> "
const thg: string "Thing"
print thg

print "Testing functions:"

set n [add n c]
print n
print "add: expect 6 -> " [add  1 [add 2 3]]
print "add: expect 4 -> " [add -1 [add 2 3]]
print "sub: expect 4 -> " [sub [add 2 3] 1]
print [lt 1 2]
print [lt 2 1]

print "Testing loop:"

var i: int 0
const iterations: int = 10
const halfway: int [idiv iterations 2]

while [lt i iterations] {
    if [eq i 0] {
        write "Attempting resolution of dotted expression within an 'if' within a 'while' block: "
        print knum.x
    }
    write "i = " i " -> "
    set i [add i 1]
    write "i' = " i
    if [eq [mod i 2] 0] {
        write " (even)"
    } else {
        write " (odd)"
    }
    print
    if [eq i halfway] {
        print "Halfway through!"
    }
}

print "Loop has completed."
var msg: string = [cat "The final value" " of the variable 'i' is " i ". Nifty, huh?"] 
print msg
print

print false
print true
print

print "Example of a variable within a namespace, using dot: " knum.x
print "Assigning to it..."
set knum.x [add 40 2]
# set [dot knum x] 42
print "New value: " knum.x
print

print "Example of a variable within a namespace, using rawattr: " [rawattr knum x]
print "Assigning to it..."
set [rawattr knum x] [add knum.x 1] 
print "New value: " knum.x
print

print "Testing two different versions of 'dot' (access to value):"
print knum.one
print delegator.one.two.three
print "---"
print [dot knum two]
print [dot delegator two]
print [rawattr delegator two]
print "Testing 'rawattr':"
print [rawattr knum three]
print

print "Testing list as head of evaluation:"
print [[idty cat] "Hello, " "world!"]
[idty print] [[idty cat] "Hello, " "world!"]
print

print "Testing delegation of setter:"
print [set [dot delegator "Hello" "world" 1 2 3] x]
var result_of_set: string [set delegator.two "Hello"]
print result_of_set
print

print "Testing quote-node (just assignment)"
const myNode = [quote-node [hello world "hi" 3]]

print "Testing empty expression: " []
