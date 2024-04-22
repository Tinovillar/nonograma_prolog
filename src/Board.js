import React from 'react';
import Square from './Square';
import Clue from './Clue';

function Board({ grid, rowsClues, colsClues, onClick, rowSat, colSat}) {
    const numOfRows = grid.length;
    const numOfCols = grid[0].length;
    return (
        <div className="flex-column">
            <div className={`flex justify-around`}>
                <div className='w-12 h-12 m-11'>{/* top-left corner square */}</div>
                {colsClues.map((clue, i) =>
                    <Clue clue={clue} key={i} isColumn={true} done={colSat.includes(i)}/>
                )}
            </div>
            <div className="flex">
                <div className={`flex flex-col justify-between`}>
                    {rowsClues.map((clue, i) =>
                        <Clue clue={clue} key={i} isColumn={false} done={rowSat.includes(i)}/>
                    )}
                </div>
                <div className="grid" style={{
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
        </div>
    );
}

export default Board;