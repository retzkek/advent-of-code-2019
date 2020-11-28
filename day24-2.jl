using Test


function update(board::BitArray{3})::BitArray{3}
    @debug "input board" board
    (r, c, l) = size(board)
    # every minute bugs will expand to new layers on each end
    new = falses(r, c, l+2)
    for k in 1:l+2
        neighbors = zeros(r, c)
        for i in 1:c
            for j in 1:r
                if (i,j) == (3,3)
                    continue
                end
                n = 0::Int64
                # left
                if i == 1
                    n += k > 2 && board[3,2,k-2]
                elseif i == 4 && j == 3
                    n += k < l+1 && sum(board[:,5,k])
                else
                    n += k > 1 && k < l+2 && board[j,i-1,k-1]
                end
                # up
                if j == 1
                    n += k > 2 && board[2,3,k-2]
                elseif j == 4 && i == 3
                    n += k < l+1 && sum(board[5,:,k])
                else
                    n += k > 1 && k < l+2 && board[j-1,i,k-1]
                end
                # right
                if i == c
                    n += k > 2 && board[3,4,k-2]
                elseif i == 2 && j == 3
                    n += k < l+1 && sum(board[:,1,k])
                else
                    n += k > 1 && k < l+2 && board[j,i+1,k-1]
                end
                # down
                if j == r
                    n += k > 2 && board[4,3,k-2]
                elseif j == 2 && i == 3
                    n += k < l+1 && sum(board[1,:,k])
                else
                    n += k > 1 && k < l+2 && board[j+1,i,k-1]
                end
                neighbors[j,i]=n
                if k > 1 && k < l+2 && board[j,i,k-1] && n != 1
                    # A bug dies (becoming an empty space) unless there is exactly
                    # one bug adjacent to it.
                    new[j,i,k] = false
                elseif k > 1 && k < l+2 && !board[j,i,k-1] && n in (1,2)
                    # An empty space becomes infested with a bug if exactly one or
                    # two bugs are adjacent to it.
                    new[j,i,k] = true
                elseif k in (1,l+2) && n in (1,2)
                    # new bug in new layers
                    new[j,i,k] = true
                elseif k > 1 && k < l+2
                    new[j,i,k] = board[j,i,k-1]
                end
            end
        end
        @debug "neighbors k=$(k)" neighbors
    end
    @debug "updated board" new
    new
end

function test()
    cells = reshape(BitArray([
        0 0 0 0 1;
        1 0 0 1 0;
        1 0 0 1 1;
        0 0 1 0 0;
        1 0 0 0 0]),5,5,1)
    cells = update(cells)
    @test cells[:,:,2] == BitArray([
        1 0 0 1 0;
        1 1 1 1 0;
        1 1 0 0 1;
        1 1 0 1 1;
        0 1 1 0 0])
    for _ in 2:10
        cells = update(cells)
    end
    @test cells[:,:,6] == BitArray([
        0 0 1 0 0;
        0 1 0 1 0;
        0 0 0 0 1;
        0 1 0 1 0;
        0 0 1 0 0])
    @test cells[:,:,7] == BitArray([
        0 0 0 1 0;
        0 0 0 1 1;
        0 0 0 0 0;
        0 0 0 1 1;
        0 0 0 1 0])
    @test cells[:,:,8] == BitArray([
        1 0 1 0 0;
        0 1 0 0 0;
        0 0 0 0 0;
        0 1 0 0 0;
        1 0 1 0 0])
    @test cells[:,:,9] == BitArray([
        0 1 0 1 1;
        0 0 0 0 1;
        0 0 0 0 1;
        0 0 0 1 1;
        0 1 1 1 0])
    @test cells[:,:,10] == BitArray([
        1 0 0 1 1;
        0 0 0 1 1;
        0 0 0 0 0;
        0 0 0 1 0;
        0 1 1 1 1])
    @test cells[:,:,11] == BitArray([
        0 1 0 0 0;
        0 1 0 1 1;
        0 1 0 0 0;
        0 0 0 0 0;
        0 0 0 0 0])
    @test cells[:,:,12] == BitArray([
        0 1 1 0 0;
        1 0 0 1 1;
        0 0 0 0 1;
        1 1 0 1 1;
        1 1 1 1 1])
    @test cells[:,:,13] == BitArray([
        1 1 1 0 0;
        1 1 0 1 0;
        1 0 0 0 0;
        0 1 0 1 1;
        1 0 1 0 0])
    @test cells[:,:,14] == BitArray([
        0 0 1 1 1;
        0 0 0 0 0;
        1 0 0 0 0;
        1 0 0 0 0;
        1 0 0 0 1])
    @test cells[:,:,15] == BitArray([
        0 1 1 1 0;
        1 0 0 1 0;
        1 0 0 0 0;
        1 1 0 1 0;
        0 0 0 0 0])
    @test cells[:,:,16] == BitArray([
        1 1 1 1 0;
        1 0 0 1 0;
        1 0 0 1 0;
        1 1 1 1 0;
        0 0 0 0 0])
    @test sum(cells) == 99
end
test()

function main()
    cells = reshape(BitArray([
        1 0 1 1 0;
        1 1 1 0 1;
        1 0 0 0 1;
        1 1 0 0 1;
        0 1 0 0 0]),5,5,1)
    for _ in 1:200
        cells = update(cells)
    end
    println(sum(cells))
end

main()
