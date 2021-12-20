module Life
    def self.dead_matrix(sz)
        return [[0]*sz]*sz
    end

    def self.next_generation(matrix)
        matrix.each_with_index do |row, y|
            row.each_with_index do |cell, x|
                puts "I'm here: (y,x)=(#{y},#{x}) cell=#{cell}"
                # num_live = count_living_neighbors(matrix, x, y)
            end
        end
        return dead_matrix(matrix.size)
    end

    def self.count_living_neighbors(matrix, x, y)
    end

    def self.get_neighbors(matrix, x, y)
        neighbors = [
            [-1, -1],
            [ 0, -1],
            [ 1, -1],

            [-1,  0],
            # [0, 0] you are here
            [ 1,  0],

            [-1,  1],
            [ 0,  1],
            [ 1,  1],
        ]
        return neighbors.map do |xoffset, yoffset|
            puts "xoffset=#{xoffset} yoffset=#{yoffset}"
            xx = x + xoffset
            xx = xx % matrix.size
            yy = y + yoffset
            yy = yy % matrix.size
            matrix[yy][xx]
        end
    end


end
