import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import SwitchBtn from './SwitchBtn';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [painting, setPainting] = useState(true);
  const [rowSat, setRowSat] = useState([]);
  const [colSat, setColSat] = useState([]);

  useEffect(() => {
    // Creation of the pengine server instance.    
    // This is executed just once, after the first render.    
    // The callback will run when the server is ready, and it stores the pengine instance in the pengine variable. 
    PengineClient.init(handleServerReady);
  }, []);

  function handleServerReady(instance) {
    pengine = instance;
    const queryS = 'init(RowClues, ColumClues, Grid)';
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['Grid']);
        setRowsClues(response['RowClues']);
        setColsClues(response['ColumClues']);
      }
    });
  }

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting) {
      return;
    }
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    const content = painting ? '#' : 'X'; // Content to put in the clicked square.
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
    setWaiting(true);
    pengine.query(queryS, (success, response) => {
      if (success) {
        setGrid(response['ResGrid']);
        if(response['RowSat']) {
          setRowSat([...rowSat, i]);
        } else {
          setRowSat(rowSat.filter(e => e !== i));
        }
        if(response['ColSat']) {
          setColSat([...colSat, j]);
        } else {
          setColSat(colSat.filter(e => e !== j));
        }
      }
      setWaiting(false);
    });
  }

  if (!grid) {
    return null;
  }
  
  return (
    <div className="flex justify-center mt-10 items-center h-min justify-evenly">
      <div className='flex flex-col justify-center items-center'>
        <h1 className='text-8xl pb-40 font-mono'>NONOGRAM.</h1>
        <SwitchBtn painting={painting} onClick={() => setPainting(!painting)}/>
      </div>
      <div className='p-4 flex rounded-md shadow-2xl border-2'>
        <Board
          grid={grid}
          rowsClues={rowsClues}
          colsClues={colsClues}
          onClick={(i, j) => handleClick(i, j)}
          rowSat={rowSat}
          colSat={colSat}
        />
      </div>
    </div>
  );
}

export default Game;