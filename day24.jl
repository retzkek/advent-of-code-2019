using Test

function update(board::BitArray{2})::BitArray{2}
    @debug board
    (r, c) = size(board)
    new = copy(board)
    for i in 1:c
        for j in 1:r
            v = [board[max(j-1,1):min(j+1,r),i]; board[j,max(i-1,1):min(i+1,c)]]
            n = sum(v)
            @debug v n
            if board[j,i] && n != 3
                # A bug dies (becoming an empty space) unless there is exactly
                # one bug adjacent to it.
                # we use 3 because the bug itself was double-counted
                new[j,i] = false
            elseif !board[j,i] && n in (1,2)
                # An empty space becomes infested with a bug if exactly one or
                # two bugs are adjacent to it.
                new[j,i] = true
            end
        end
    end
    @debug new
    new
end


cells = BitArray([0 0 0 0 1;
                  1 0 0 1 0;
                  1 0 0 1 1;
                  0 0 1 0 0;
                  1 0 0 0 0])
cells = update(cells)
@test cells == BitArray([1 0 0 1 0;
                         1 1 1 1 0;
                         1 1 1 0 1;
                         1 1 0 1 1;
                         0 1 1 0 0])
cells = update(cells)
@test cells == BitArray([1 1 1 1 1;
                         0 0 0 0 1;
                         0 0 0 0 1;
                         0 0 0 1 0;
                         1 0 1 1 1])
cells = update(cells)
@test cells == BitArray([1 0 0 0 0;
                         1 1 1 1 0;
                         0 0 0 1 1;
                         1 0 1 1 0;
                         0 1 1 0 1])
cells = update(cells)
@test cells == BitArray([1 1 1 1 0;
                         0 0 0 0 1;
                         1 1 0 0 1;
                         0 0 0 0 0;
                         1 1 0 0 0])


weights = transpose(reshape([2^x for x=0:24],5,5))
biodiversity(board::BitArray{2})::Int64 = sum(board .* weights)

@test biodiversity(BitArray([0 0 0 0 0;
                             0 0 0 0 0;
                             0 0 0 0 0;
                             1 0 0 0 0;
                             0 1 0 0 0])) == 2129920

function main()
    cells = BitArray([1 0 1 1 0;
                      1 1 1 0 1;
                      1 0 0 0 1;
                      1 1 0 0 1;
                      0 1 0 0 0])
    seen = Set()
    b = 0::Int64
    while !((b=biodiversity(cells)) in seen)
        @debug b
        push!(seen,b)
        @debug seen
        cells = update(cells)
    end
    println(b)
end

main()
