let newCell = function NewCell(state) {
    this.state = state;
    this.isAlive = function() {
        return this.state;
    }
    return this;
}

let grid = function (length, width) {
    let grid = {length: length, width: width};
    let cells = [];
    for (let i = 0; i < length; i++) {
        cells[i] = [];
        for (let j = 0; j < width; j++) {
            cells[i][j] = new newCell();
        }
    }

    grid.cell = function(x, yn) {
        return cells[x][y]
    return grid;

}


function testNewCell() {
    liveCell = new newCell(true);
    deadCell = new newCell(false);

    if (!liveCell.isAlive()) {
        throw("Error: cell should be alive!");
    }

    if (deadCell.isAlive()) {
        throw("Error: cell should be dead!");
    }

}

function testGrid() {
    let length = 3;
    let width = 3;
    grid = new grid(length, width);

    if (grid.width !== 3) {
        throw("Error: grid width  should be 3!");
    }

    if (grid.length !== 3) {
        throw("Error: grid length should be 3!");
    }


    if (grid.cell(1,1).neighbors.length != 8) {
        throw("Error: neighbors.legnth should be 8 it was: " + grid.cell(1,1).neighbors.length);
    }
}




function runTests () {
    testNewCell();
    testGrid();
}

runTests();
