import React from 'react';

function Clue({ clue, isColumn, done }) {
    return (
        <div className={`flex ${isColumn ? "flex-col" : ''} items-center justify-evenly text-center w-20 h-20 ${done ? 'bg-green-400' : 'bg-blue-200'} rounded-md m-1 border`} >
            {clue.map((num, i) =>
                <div key={i} className='text-lg w-auto h-auto'>
                    {num}
                </div>
            )}
        </div>
    );
}



export default Clue;