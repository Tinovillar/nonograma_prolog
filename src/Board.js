import React, { useEffect } from 'react';
import Square from './Square';
import Clue from './Clue';

function Board({ grid, rowsClues, colsClues, onClick, onLoad, onVictory, rowSat, colSat}) {
    const numOfRows = grid.length;
    const numOfCols = grid[0].length;

    useEffect(() => {
        onLoad(); 
    },[]);

    return (
        <div className="grid grid-rows-4 grid-cols-4 items-center align-center justify-items-center w-full">
            <div className={`flex col-start-2 col-span-4 w-full h-full`}>
                {colsClues.map((clue, i) =>
                    <Clue clue={clue} key={i} isColumn={true} done={colSat.includes(i)}/>
                )}
            </div>
            <div className={`flex flex-col row-start-2 row-span-4 w-full h-full`}>
                {rowsClues.map((clue, i) =>
                    <Clue clue={clue} key={i} isColumn={false} done={rowSat.includes(i)}/>
                )}
            </div>
            <div className="grid row-start-2 row-span-4 col-start-2 col-span-4 w-full h-full" style={{
                    gridTemplateColumns: `repeat(${numOfCols}, minmax(0, 1fr))`,
                    gridTemplateRows: `repeat(${numOfRows}, minmax(0,1fr))`
                }}
            >
                {grid.map((row, i) =>
                    row.map((cell, j) =>
                        <Square
                            value={cell}
                            onClick={() => onClick(i, j)}
                            key={i + j}
                        />
                    )
                )}
            </div>
        </div>
    );
}

export default Board;