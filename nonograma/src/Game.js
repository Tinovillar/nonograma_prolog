import React, { useEffect, useState } from 'react';
import PengineClient from './PengineClient';
import Board from './Board';
import SwitchBtn from './SwitchBtn';

let pengine;

function Game() {

  // State
  const [grid, setGrid] = useState(null);
  const [solvedGrid, setSolvedGrid] = useState([]);
  const [rowsClues, setRowsClues] = useState(null);
  const [colsClues, setColsClues] = useState(null);
  const [waiting, setWaiting] = useState(false);
  const [painting, setPainting] = useState(true);
  const [showSquareState, setShowSquareState] = useState(false);
  const [mode, setMode] = useState(true);
  const [rowSat, setRowSat] = useState([]);
  const [colSat, setColSat] = useState([]);
  const [victory, setVictory] = useState(false);
  const [cant, setCant] = useState(0);
  const [cantAll, setCantAll] = useState(0);

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

  function handleInitialChecks() {
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_');
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `initial_check(${rowsCluesS}, ${colsCluesS}, ${squaresS}, RowsCluesChecked, ColsCluesChecked, Cantidad)`;
    pengine.query(queryS, (success, response) => {
      if (success) {
        for (let index = 0; index < colsClues.length; index++) {
          if(response['ColsCluesChecked'][index]) {
            setColSat([...colSat, index]);
          }
        }
        for (let index = 0; index < rowsClues.length; index++) {
          if(response['RowsCluesChecked'][index]) {
            setRowSat([...rowSat, index]);
          }
        }
        setCant(response['Cantidad']);
      }
    })
    solve();
  }

  function handleVictory() {
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_');
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `victory_check(${rowsCluesS}, ${colsCluesS}, ${squaresS})`;
    pengine.query(queryS, (success, response) => {
      if (success) {
        setVictory(true);
      }
    })
  }

  function solve() {
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_');
      const rowsCluesS = JSON.stringify(rowsClues);
      const colsCluesS = JSON.stringify(colsClues);
      const queryS = `solve(${rowsCluesS}, ${colsCluesS}, ${squaresS}, Solved, CantidadCorrecta)`;
      pengine.query(queryS, (success, response) => {
        if (success && !victory) {
          setSolvedGrid(response['Solved']);
          setCantAll(response['CantidadCorrecta']);
        }
      })
  }

  function handleClickQuery(i, j, content) {
    const squaresS = JSON.stringify(grid).replaceAll('"_"', '_'); // Remove quotes for variables. squares = [["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]]
    const rowsCluesS = JSON.stringify(rowsClues);
    const colsCluesS = JSON.stringify(colsClues);
    const queryS = `put("${content}", [${i},${j}], ${rowsCluesS}, ${colsCluesS}, ${squaresS}, ResGrid, RowSat, ColSat, Cantidad)`; // queryS = put("#",[0,1],[], [],[["X",_,_,_,_],["X",_,"X",_,_],["X",_,_,_,_],["#","#","#",_,_],[_,_,"#","#","#"]], GrillaRes, FilaSat, ColSat)
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
        setCant(response['Cantidad']);
      }
      setWaiting(false);
    });
  }

  function handleClick(i, j) {
    // No action on click if we are waiting.
    if (waiting || victory) {
      return;
    }
    // Build Prolog query to make a move and get the new satisfacion status of the relevant clues.    
    let content = mode ? '#' : 'X';
    if(!showSquareState)
      handleClickQuery(i,j, content);
    else
      handleSquareState(i,j);
  }

  function handleSquareState(i,j) {
    if (solvedGrid[i][j] === '#' && grid[i][j] !== '#') {
      handleClickQuery(i,j,'#');
    } else if (solvedGrid[i][j] !== '#' && grid[i][j] !== 'X'){
      handleClickQuery(i,j,'X');
    }
    setShowSquareState(!showSquareState);
  }

  if (!grid) {
    return null;
  }

  const updatePage = () => {
    window.location.reload();
  };
  
  return (
    <>
      <div className={`${!victory ? 'hidden' : ''} fixed w-2/5 h-2/5 top-1/2 left-1/2 transform -translate-x-1/2 -translate-y-1/2 shadow-md`}>
        <div className='w-full h-full flex flex-col justify-evenly bg-yellow-300 font-mono rounded-xl'>
          <h1 className='text-8xl text-center'>Ganaste!</h1>
          <button type="button" className='bg-white w-1/3 ml-auto mr-auto text-xl animate-bounce rounded-md p-2' onClick={updatePage}>Intentar de nuevo</button>
        </div>
      </div>
      <div className="flex items-center h-screen justify-evenly">
        <div className='flex flex-col justify-center items-center'>
            <h1 className='text-8xl pb-20 font-mono'>NONOGRAM.</h1>
            <div className='flex flex-row items-center justify-evenly'>
              <h3 className='text-4xl pb-20 font-mono'>{cant}/{cantAll}</h3>
              <i className="fa-solid fa-square-check -mt-20 ml-3 fa-2x"></i>
            </div>
          <div className='flex w-full justify-between'>
            <div onClick={() => setShowSquareState(!showSquareState)} className={`flex flex-col cursor-pointer rounded-3xl ${!showSquareState ? "shadow-xl" : "border-transparent bg-gray-200"} border-2 mr-10`}>
                <h3 className={`text-4xl font-mono p-4`}>Reveal</h3>
                <button type='button'><i className="fa-solid fa-lightbulb fa-2x pb-4"></i></button>
            </div>
            <div className='flex flex-col mt-6'>
                <SwitchBtn mode={mode} onClick={() => setMode(!mode)}/>
            </div>
            <div onClick={() => setPainting(!painting)} className={`flex flex-col cursor-pointer rounded-3xl ${painting ? "shadow-xl" : "border-transparent bg-gray-200"} border-2 ml-10`}>
                <h3 className={`text-4xl font-mono p-4`}>Solve</h3>
                <button type='button' ><i className="fa-solid fa-circle-check fa-2x pb-4"></i></button>
            </div>
          </div>
        </div>
        <div className='p-4 flex rounded-md shadow-2xl border-2 square w-5/12'>
          <Board
            grid={grid}
            rowsClues={rowsClues}
            colsClues={colsClues}
            onClick={(i, j) => handleClick(i, j)}
            onLoad={() => handleInitialChecks()}
            onVictory={() => handleVictory()}
            rowSat={rowSat}
            colSat={colSat}
            solvedGrid={solvedGrid}
            painting={painting}
          />
        </div>
      </div>
    </>
  );
}

export default Game;