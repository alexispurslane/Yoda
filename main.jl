operators = {
"+" => +,
"-" => -,
"*" => *,
"/" => /,
"%" => mod,
"^" => ^,
".." => function (x, y)
    string(x) * string(y)
end,
"println" => function (x)
    println(x)
    x
end,
"drop" => function (x)
end,
"call" => function (x)
    x()
    nothing
end
}

opnums = {
"+" => 2,
"-" => 2,
"*" => 2,
"/" => 2,
"%" => 2,
"^" => 2,
".." => 2,
"println" => 1,
"drop" => 1,
"call" => 1
}

stack = Any[]
block = Any[]

function rpn(s)
    in_block = false
    for op in convert(Array{ASCIIString}, split(s))
        if op == "["
            in_block = true
            while length(block) > 0
                pop!(block)
            end
        elseif op == "]"
            in_block = false

            push!(stack, function ()
                  blk = deepcopy(block)
                  rpn(join(blk, " "))
                  end)
        elseif op == "def"
            nargs = 0
            quot = pop!(stack)
            name = pop!(stack)

            operators[name] = function ()
                quot()
            end
            opnums[name] = nargs
        elseif !in_block && op[1] == ':'
            push!(stack, op[2:length(op)])
        elseif !in_block
            f = get(operators, op, false)
            if f != false
                if length(stack) < get(opnums, op, 1)
                    println("ERROR: Data stack underflow")
                else
                    args = Any[]
                    for i in 1:get(opnums, op, 1)
                        push!(args, pop!(stack))
                    end

                    res = apply(f, args)
                    if res != nothing
                        push!(stack, res)
                    end
                end
            elseif op == "clear"
                while length(stack) != 0
                    pop!(stack)
                end
            elseif typeof(parse(op)) == Int
                push!(stack, parse(op))
            elseif typeof(parse(op)) == ASCIIString
                push!(stack, parse(op))
            else
                println("ERROR: Calling undefined function")
            end
        else
            push!(block, op)
        end
    end
    
    if length(stack) > 0
        return stack[length(stack)]
    else
        ""
    end
end

while true
    print("> ")
    input = chomp(readline())
    if input == "quit"
        break;
    else
        rpn(input)
        
        println("--- DATA STACK ---\n")
        for el in stack
            if typeof(el) == ASCIIString
                println("\"", el, "\"")
            else
                println(el)
            end
        end
    end
end
