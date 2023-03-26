
function test(t)
    a = 0
    Threads.@threads for i in 1:t
    a += i
    #println(a, " on thread ", Threads.threadid())
    end
end
function test1(t)
    a = 0
    for i in 1:t
    a += i
    #println(a, " on thread ", Threads.threadid())
    end
end

a = 0
@time test(1000000000)
@time test1(1000000000)
