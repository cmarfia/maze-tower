void function () {
    const elem = document.getElementById("maze-tower__game");
    const numRows = 5;
    const numCols = 5;

    const createSVGNode = (name) => (attributes = [], children = []) => {
        const elem = document.createElementNS("http://www.w3.org/2000/svg", name);

        for (let a = 0; a < attributes.length; a++) {
            const [name, value] = attributes[a];
            elem.setAttribute(name, value);
        }

        for (let c = 0; c < children.length; c++) {
            elem.appendChild(children[c]);
        }

        return elem;
    };

    // Helper functions
    const svg = createSVGNode('svg');
    const g = createSVGNode('g');
    const line = createSVGNode('line');
    const rect = createSVGNode('rect');

    const render = (matrix, entrance, exit) =>
        /*
            Example matrix (rows (row (cell))):
            [ [ (0,0), (0,1), (0,2)]
            , [ (1,0), (1,1), (1,2)]
            , [ (2,0), (2,1), (2,2)]
            ]

            SVG is transposed:
                (0,0) → X → (N,0)
                ↓
                Y
                ↓
                (0,N)
        */
        elem.append(
            svg([['viewBox', `0 0 ${matrix[0].length} ${matrix.length}`]], [
                g([], [rect([["x", entrance.col], ["y", entrance.row], ["width", 1], ["height", 1], ["style", "fill:rgb(0,255,0)"]])]),
                g([], [rect([["x", exit.col], ["y", exit.row], ["width", 1], ["height", 1], ["style", "fill:rgb(255,0,0)"]])]),
                g([['fill', 'none'], ['stroke', '#000000'], ['stroke-width', '.1'], ['stroke-linecap', 'square']], [
                    line([['x1', '0'], ['y1', '0'], ['x2', '0'], ['y2', matrix[0].length]]),
                    line([['x1', '0'], ['y1', '0'], ['x2', matrix.length], ['y2', '0']]),
                    line([['x1', matrix.length], ['y1', matrix[0].length], ['x2', '0'], ['y2', matrix[0].length]]),
                    line([['x1', matrix.length], ['y1', matrix[0].length], ['x2', matrix.length], ['y2', '0']]),
                    ...matrix.reduce((rows, row, rowIndex) =>
                        rows.concat(row.reduce((cells, cell, colIndex) =>
                            cells.concat(
                                ...(cell.up ? [] : [line([['x1', colIndex], ['y1', rowIndex], ['x2', colIndex + 1], ['y2', rowIndex]])]),
                                ...(cell.right ? [] : [line([['x1', colIndex + 1], ['y1', rowIndex], ['x2', colIndex + 1], ['y2', rowIndex + 1]])])
                            ), [])), [])
                ])
            ]));

    const logMaze = (maze, entrance, exit) => {
        const isEntrance = (row, col) => entrance.row === row && entrance.col === col;
        const isExit = (row, col) => exit.row === row && exit.col === col;
        console.log('entrance', entrance);
        console.log('exit', exit);
        console.table(
            maze.map((row, rowIndex) =>
                row.map((cell, colIndex) =>
                    `${isEntrance(rowIndex, colIndex) ? 'S' : ''}${isExit(rowIndex, colIndex) ? 'E' : ''}${cell.left ? '←' : '-'} ${cell.up ? '↑' : '-'} ${cell.down ? '↓' : '-'} ${cell.right ? '→' : '-'}`)));
    }

    fetch(`/mazes/${numRows}/${numCols}`)
        .then(res => res.json())
        .then(({ entrance, exit, maze }) => {
            logMaze(maze, entrance, exit)
            render(maze, entrance, exit)
        })
        .catch((err) => console.error(err));
}();
