import React from 'react';
import Square from './Square';
import Clue from './Clue';

function Board({ grid, rowsClues, colsClues, onClick }) {
    const numOfRows = grid.length;
    const numOfCols = grid[0].length;
    return (
        <div className="flex-column">
            <div className={`flex justify-around`}>
                <div className='w-20 h-20 m-1'>{/* top-left corner square */}</div>
                {colsClues.map((clue, i) =>
                    <Clue clue={clue} key={i} isColumn={true} />
                )}
            </div>
            <div className="flex">
                <div className={`flex flex-col justify-between`}>
                    {rowsClues.map((clue, i) =>
                        <Clue clue={clue} key={i} isColumn={false} />
                    )}
                </div>
                <div className={`grid grid-cols-${numOfCols} grid-rows-${numOfRows}`}>
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
        </div>
    );
}

export default Board;