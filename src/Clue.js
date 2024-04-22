import React from 'react';

function Clue({ clue, isColumn, done }) {
    return (
        <div className={`flex ${isColumn ? "flex-col h-32 w-12" : 'w-32 h-12'} items-center justify-evenly text-center ${done ? 'bg-green-400' : 'bg-blue-200'} rounded-md m-1 border`} >
            {clue.map((num, i) =>
                <div key={i} className='text-lg'>
                    {num}
                </div>
            )}
        </div>
    );
}

export default Clue;