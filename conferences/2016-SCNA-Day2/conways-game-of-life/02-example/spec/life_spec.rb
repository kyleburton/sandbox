require 'life'

RSpec.describe Life, "The Game of Life" do
    it "should make an empty matrix" do
        matrix = Life.dead_matrix(10)
        expect(matrix.size).to    eq 10
        expect(matrix[0].size).to eq 10
    end

    it "should stay dead if it is dead" do
        dead_matrix = Life.dead_matrix(10)
        next_matrix = Life.next_generation(dead_matrix)
        expect(dead_matrix).to eq next_matrix
    end

    context "count_living_neighbors" do
        xit "should return 0 for every cell in a dead matrix" do
            matrix = Life.dead_matrix(5)
            5.times do |x|
                5.times do |y|
                    expect(Life.count_living_neighbors(matrix, y, x)).to == 0
                end
            end
        end

        it "should return the set of neighbors" do
            matrix = [
                [ 1,   2,   3,   4,   5],
                [ 6,   7,   8,   9,  'a'],
                ['b', 'c', 'd', 'e', 'f'],
                ['g', 'h', 'i', 'j', 'k'],
                ['l', 'm', 'n', 'o', 'p'],
            ]

            expected_neighbors = [7, 8, 9, 'c', 'e', 'h', 'i', 'j']
            expect(Life.get_neighbors(matrix, 2, 2)).to eq expected_neighbors

            matrix = [
                [1, 2, 3],
                [4, 5, 6],
                [7, 8, 9]
            ]

            expected_neighbors = [3, 1, 2, 6, 5, 9, 7, 8]
            expect(Life.get_neighbors(matrix, 0, 1)).to eq expected_neighbors

            expected_neighbors = [9, 7, 8, 3, 2, 6, 4, 5]
            expect(Life.get_neighbors(matrix, 0, 0)).to eq expected_neighbors

        end
    end

    it "1. Any Live Cell with < 2 Live Neighbors dies of under-population" do
        initial_state = [
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 1, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
        ]

        next_matrix = Life.next_generation(initial_state)
        expect(next_matrix).to eq Life.dead_matrix(5)
    end

    xit "2. Any live cell with 2 or 3 live neighbors lives on" do
        initial_state = [
            [0, 0, 0, 0, 0],
            [0, 1, 1, 0, 0],
            [0, 1, 1, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
        ]

        expected_state = [
            [0, 0, 0, 0, 0],
            [0, 1, 1, 0, 0],
            [0, 1, 1, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
        ]

        next_matrix = Life.next_generation(initial_state)
        expect(next_matrix).to eq expected_state
    end

    xit "3. Any live cell with > 3 live neighbors dies of over population" do
    end

    xit "4. Any dead cell with exactly 3 live neighbors comes alive" do
    end

end
